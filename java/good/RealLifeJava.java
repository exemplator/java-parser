package xyz.exemplator.exemplator;

import com.github.javaparser.Position;
import org.apache.http.HttpException;
import ratpack.exec.Promise;
import ratpack.jackson.Jackson;
import ratpack.server.RatpackServer;
import ratpack.server.ServerConfig;
import xyz.exemplator.exemplator.data.CodeSample;
import xyz.exemplator.exemplator.data.ICodeSearch;
import xyz.exemplator.exemplator.parser.Parsers;
import xyz.exemplator.exemplator.parser.Selection;
import xyz.exemplator.exemplator.parser.java.Command;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;
import static ratpack.jackson.Jackson.fromJson;

/**
 * @author LeanderK
 * @version 1.0
 */
public class Router {
    private final int port;
    private final ICodeSearch codeSearch;
    private ExecutorService executorService = Executors.newCachedThreadPool();

    public Router(int port, ICodeSearch codeSearch) {
        this.port = port;
        this.codeSearch = codeSearch;
    }

    public void init() throws Exception {
        RatpackServer.start(server -> server
                .serverConfig(ServerConfig.embedded().port(port))
                .handlers(chain ->
                        chain.all(ctx -> {
                            ctx.getResponse().getHeaders().add("access-control-allow-origin", "*");
                            ctx.getResponse().getHeaders().add("access-control-allow-methods", "GET,PUT,POST,PATCH,DELETE,OPTIONS");
                            ctx.getResponse().getHeaders().add("access-control-allow-credentials", "true");
                            ctx.getResponse().getHeaders().add("access-control-allow-headers", "Authorization,Content-Type");
                            ctx.getResponse().getHeaders().add("access-control-expose-headers", "Link,Location");
                            ctx.getResponse().getHeaders().add("access-control-max-age", "86400");
                            ctx.next();
                        })
                        .options(ctx -> {
                            ctx.getResponse().status(204);
                            ctx.getResponse().contentType("text/plain");
                            ctx.render("");
                        })
                        .post("search", ctx -> {
                            if (!ctx.getRequest().getContentType().isJson()) {
                                ctx.getResponse().status(500);
                                ctx.render("Expected Content-Type: application/json");
                                return;
                            }
                            ctx.render(ctx.parse(fromJson(Request.class))
                                    .flatMap(request -> {
                                        if (!request.getToken().isPresent()
                                                && !request.getClassName().isPresent() && !request.getMethodName().isPresent()) {
                                            throw new IllegalArgumentException("token, methodName or classname must be present");
                                        }
                                        if (request.getMethodName().map(name -> name.equals("new") && !request.getClassName().isPresent()).orElse(false)) {
                                            throw new IllegalArgumentException("searching for constructor must specify the class");
                                        }
                                        List<String> searchTerms = Stream.of(request.getClassName(), request.getMethodName(), request.getPackageName(), request.getToken())
                                                .filter(Optional::isPresent)
                                                .map(Optional::get)
                                                .collect(toList());
                                        Command command = new Command(request.getClassName().orElse(null), request.getPackageName().orElse(null), request.getMethodName().orElse(null));
                                        return Promise.<ResultAndPage>async(downstream -> downstream.accept(handleSearchRequest(searchTerms, request.getPage(), executorService, command, request, 0, new HashSet<>(), 0)))
                                                .map((ResultAndPage resultAndPage) -> {
                                                    List<Response.Occurrence> occurrences = resultAndPage.samples.stream()
                                                            .map(this::createOccurence)
                                                            .collect(toList());
                                                    return new Response(occurrences, resultAndPage.startPage, resultAndPage.endPage);
                                                })
                                                .map(Jackson::json);
                                    }));
                        })
                        .get("version", ctx -> ctx.render(getClass().getPackage().getImplementationVersion()))
                )
        );
    }

    public CompletableFuture<ResultAndPage> handleSearchRequest(
            List<String> searchTerms, int requestPage, ExecutorService executorService,
            Command command, Request request, int accumulator, Set<Integer> selectionHashes, int recursion) throws HttpException {
        return doCodeSearch(searchTerms, requestPage, executorService, command, request, selectionHashes)
                .thenCompose((List<CodeSample> results) -> {
                    int newAccumulator = accumulator + results.size();
                    if (newAccumulator < 10 && recursion <= 5) {
                        try {
                            return handleSearchRequest(searchTerms, requestPage + 1, executorService, command, request, newAccumulator, selectionHashes, recursion + 1)
                                    .<ResultAndPage>thenApply(resultAndPage -> {
                                        if (results.isEmpty()) {
                                            return resultAndPage;
                                        } else {
                                            resultAndPage.samples.addAll(results);
                                            return new ResultAndPage(resultAndPage.samples, requestPage, resultAndPage.endPage);
                                        }
                                    });
                        } catch (HttpException e) {
                            throw new RuntimeException(e);
                        }
                    } else {
                        return CompletableFuture.completedFuture(new ResultAndPage(results, requestPage, requestPage));
                    }
                });
    }

    public CompletableFuture<List<CodeSample>> doCodeSearch(List<String> searchTerms, int requestPage, ExecutorService executorService,
                                                            Command command, Request request, Set<Integer> selectionHashes) throws HttpException {
        CompletableFuture<List<Optional<CodeSample>>> fetch = codeSearch.fetch(searchTerms, requestPage, executorService);


        return fetch.thenApply(v ->
                v.stream()
                        .map(opt -> opt.flatMap(sample -> parseOrReturn(sample, command, request)))
                        .filter(Optional::isPresent)
                        .map(Optional::get)
                        .filter(codeSample -> !codeSample.getSelections().isEmpty())
                        .filter(codeSample -> {
                            Map<Selection, String> surroundings = codeSample.getSurroundings();
                            //only first selection gets shown
                            int hashCode = surroundings.get(codeSample.getSelections().get(0)).hashCode();
                            return selectionHashes.add(hashCode);
                        })
                        .collect(Collectors.toList())
        );
    }

    public Optional<CodeSample> parseOrReturn(CodeSample sample, Command command, Request request) {
        if (request.getToken().isPresent()) {
            sample.getSelections().add(new Selection(new Position(-1, -1), new Position(-1, -1)));
            return Optional.of(sample);
        } else {
            return Parsers.from("JAVA", sample)
                    .map(parser -> parser.getMatches(command))
                    .map(matches -> {
                        sample.setSelections(matches);
                        return sample;
                    });
        }
    }

    private Response.Occurrence createOccurence(CodeSample codeSample) {
        List<Response.Selection> selections = codeSample.getSelections().stream()
                .map(selection -> {
                    Response.Position start = new Response.Position(selection.getStart().getLine(), selection.getStart().getColumn());
                    Response.Position end = new Response.Position(selection.getEnd().getLine(), selection.getEnd().getColumn());
                    return new Response.Selection(start, end);
                })
                .collect(Collectors.toList());

        return new Response.Occurrence(codeSample.getFileUrl() , codeSample.getUserUrl(), codeSample.getCode(), selections);
    }

    private static class ResultAndPage {
        List<CodeSample> samples;
        int startPage;
        int endPage;

        public ResultAndPage(List<CodeSample> samples, int startPage, int endPage) {
            this.samples = samples;
            this.startPage = startPage;
            this.endPage = endPage;
        }
    }
}

java-parser
=============

[![Build Status](https://travis-ci.org/exemplator/java-parser.svg?branch=master)](https://travis-ci.org/exemplator/java-parser)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

Haskell parser and pretty printer for the java language. It can parse java up until java 9.
Based on [language-java](https://github.com/vincenthz), for which we are thankful, this project is expected to diverge (until now: record syntax and new AST representation, fixed some significant bugs, reorganization of the modules, lenses, java 9 support) while we add new features that are needed for [java-usage-finder](https://github.com/exemplator/java8-usage-finder).

[documentation](https://exemplator.github.io/java-parser/) 


How to use

----------

Simple compilation unit parser:

    parseCompilationUnit "import java.util.*; public class MyClass {}"

or from a file:

    ast <- parseCompilationUnit `fmap` readFile "myClass.java"

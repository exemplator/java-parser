{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Language.Java.Parser (
    P,
    Parsable(..),

    parseCompilationUnit, parser,

    compilationUnit, packageDeclParser, importDecl, typeDeclParser,

    classDeclParser, interfaceDeclParser,

    memberDecl, fieldDecl, methodDecl, constrDecl,
    interfaceMemberDecl, absMethodDecl,

    formalParams, formalParam,

    modifier,

    varDecls, varDecl,

    blockParser, blockStmt, stmt,

    stmtExp, expParser, primaryParser, literalParser,

    ttype, primType, refType, classType, resultType, typeDeclSpecifier,

    lambdaExp, methodRef,

    typeParams, typeParam,

    name, ident,

    empty, list, list1, seplist, seplist1, opt, bopt, lopt,

    comma, semiColon, period, colon

    ) where

import           Language.Java.Lexer         (L (..), Token (..), lexer)
import           Language.Java.Position
import           Language.Java.Pretty        (pretty)
import           Language.Java.Syntax
import           Language.Java.SyntaxClasses

import           Text.Parsec                 hiding (Empty)
import           Text.Parsec.Pos

import           Data.Char                   (toLower)
import           Data.Maybe                  (catMaybes, fromMaybe, isJust,
                                              listToMaybe, maybe)
import           Prelude                     hiding ((>>), (>>=))
import qualified Prelude                     as P ((>>), (>>=))

import           Control.Applicative         ((<$), (<$>), (<*), (<*>))

type P = Parsec [L Token] ()

-- A trick to allow >> and >>=, normally infixr 1, to be
-- used inside branches of <|>, which is declared as infixl 1.
-- There are no clashes with other operators of precedence 2.
(>>) = (P.>>)
(>>=) = (P.>>=)
infixr 2 >>, >>=
-- Note also when reading that <$> is infixl 4 and thus has
-- lower precedence than all the others (>>, >>=, and <|>).

-- Parsing class for type parameter

class Parsable l where
    toParser :: P (l -> a) -> P a

instance Parsable Segment where
    toParser pConstr = do
        start <- getPosition
        result <- pConstr
        end <- getPosition
        let segmt = sourcePosToSegment start end
        return (result segmt)

tP :: (Parsable l) => P (l -> a) -> P a
tP = toParser

(<$$>) :: (Parsable l) => (l -> a -> b) -> P a -> P (l -> b)
(<$$>) constr pa = pa >>= (flip constr)
infixl 4 <$$>

(<**>) :: (Parsable l) => P (l -> a -> b) -> P a -> P (l -> b)
(<**>) pconstr pa = flip <$> pconstr <*> pa
infixl 4 <**>

returnN :: Monad m HasNode a b => (l -> a) -> m (l -> b)
returnN a = return $ \l -> toNode (a l)

returnN' :: Monad m HasNode a b => a -> m b
returnN' a = return $ toNode a

wrap :: Applicative f HasNode a b => f a -> f b
wrap a = toNode <$> a

wrap :: Applicative f HasNode a b => f a -> f b
wrap a = (\a l -> toNode (a l)) <$> a

wrapM :: Applicative f HasNode a b => f (Mod a) -> f (Mod b)
wrapM a = (\a ms l -> toNode (a ms l)) <$> a

----------------------------------------------------------------------------
-- Top-level parsing

parseCompilationUnit :: String -> Either ParseError (CompilationUnit Segment)
parseCompilationUnit inp =
    runParser compilationUnit () "" (lexer inp)

parser :: P a -> String -> Either ParseError a
parser p = runParser p () "" . lexer

--class Parse a where
--  parse :: String -> a

----------------------------------------------------------------------------
-- Packages and compilation units

compilationUnitNode :: Parsable l => P (CompilationUnitNode l)
compilationUnitNode = try (wrap compilationUnit)
        <|> wrap moduleDeclaration

compilationUnit :: Parsable l => P (CompilationUnitNode l)
compilationUnit = tP $ do
    mpd <- opt packageDeclParser
    ids <- list importDecl
    tds <- list typeDeclParser
    eof
    return $ \l -> CompilationUnit l mpd ids (catMaybes tds)

moduleDeclaration :: Parsable l => P (CompilationUnitNode l)
moduleDeclaration = tP $ do
    -- only tokens in module descriptions!
    (Ident "module") <- ident
    modulePackageP <- fullQualiPkg
    moduleSpecsP <- braces $ list moduleSpecParser
    return $ \l -> ModuleDeclaration l modulePackageP moduleSpecsP

packageDeclParser :: (Parsable l) => P (PackageDecl l)
packageDeclParser = tp $ do
    pgkDec <- tP PackageDecl
    tok KW_Package
    pkg <- fullQualiPkg
    semiColon
    return $ flip PackageDecl pkg

moduleSpecParser :: (Parsable l) => P (ModuleSpecNode l)
moduleSpecParser = tP $ try (do
        -- only tokens in module descriptions!
        (Ident "requires") <- ident
        reqMod <- fullQualiPkg
        semiColon
        returnN $ flip ModuleRequires reqMod)
    <|> do
        (Ident "exports") <- ident
        exportsPkg <- fullQualiPkg
        semiColon
        returnN $ flip ModuleExports exportsPkg

importDecl :: (Parsable l) => P (ImportDecl l)
importDecl = tP $ do
    tok KW_Import
    st <- bopt $ tok KW_Static
    pkg  <- seplist1 ident period
    ds <- bopt $ period >> tok Op_Star
    let package = if ds then WildcardPackage pkg else FullQualiPackage pkg
    semiColon
    return $ \l -> ImportDecl l st package

typeDeclParser :: (Parsable l) => P (Maybe (TypeDeclNode l))
typeDeclParser = Just <$> classOrInterfaceDecl <|>
            const Nothing <$> semiColon

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

classOrInterfaceDecl :: (Parsable l) => P (TypeDeclNode l)
classOrInterfaceDecl = do
    ms <- list modifier
    de <- (do cd <- classDeclParser
              returnN $ \mst l -> ClassTypeDecl l cl (cd mst)) <|>
          (do idecl <- annInterfaceDecl <|> interfaceDeclParser
              return $ \mst l -> InterfaceTypeDecl lifT (idecl mst))
    return $ de ms

classDeclParser :: (Parsable l) => P (Mod l (ClassDecl l))
classDeclParser = normalClassDecl <|> enumClassDecl

normalClassDecl :: (Parsable l) => P (Mod l (ClassDecl l))
normalClassDecl = do
    clDec <- tP ClassDecl
    tok KW_Class
    i   <- ident
    tps <- lopt typeParams
    mex <- opt extendsParser
    imp <- lopt implementsParsre
    bod <- classBodyParser
    return $ \ms -> clDec ms i tps (fmap head mex) imp bod

extendsParser :: (Parsable l) => P [Extends l]
extendsParser = tok KW_Extends >> map <$> tP Extends <*> refTypeList

implementsParsre :: (Parsable l) => P [Implements l]
implementsParsre = tok KW_Implements >> map <$> tP Implements <*> refTypeList

enumClassDecl :: (Parsable l) => P (Mod l (ClassDecl l))
enumClassDecl = do
    enDec <- tP EnumDecl
    tok KW_Enum
    i   <- ident
    imp <- lopt implementsParsre
    bod <- enumBodyParser
    return $ \ms -> enDec ms i imp bod

classBodyParser :: (Parsable l) => P (ClassBody l)
classBodyParser = toParser ClassBody <*> braces classBodyStatements

enumBodyParser :: (Parsable l) => P (EnumBody l)
enumBodyParser = braces $ do
    enB <- tP EnumBody
    ecs <- seplist enumConst comma
    optional comma
    eds <- lopt enumBodyDecls
    return $ enB ecs eds

enumConst :: (Parsable l) => P (EnumConstant l)
enumConst = do
    enC <- tP EnumConstant
    idt  <- ident
    as  <- lopt args
    mcb <- opt classBodyParser
    return $ enC idt as mcb

enumBodyDecls :: (Parsable l) => P [DeclNode l]
enumBodyDecls = semiColon >> classBodyStatements

classBodyStatements :: (Parsable l) => P [DeclNode l]
classBodyStatements = catMaybes <$> list classBodyStatement

-- Interface declarations

annInterfaceDecl :: (Parsable l) => P (Mod l (InterfaceDecl l))
annInterfaceDecl = do
    ifD <- tP InterfaceDecl
    tok KW_AnnInterface
    idt  <- ident
    tps <- lopt typeParams
    exs <- lopt extendsParser
    bod <- interfaceBodyParser
    return $ \ms -> ifD InterfaceAnnotation ms idt tps exs bod

interfaceDeclParser :: (Parsable l) => P (Mod l (InterfaceDecl l))
interfaceDeclParser = do
    ifD <- tP InterfaceDecl
    tok KW_Interface
    idt  <- ident
    tps <- lopt typeParams
    exs <- lopt extendsParser
    bod <- interfaceBodyParser
    return $ \ms -> ifD InterfaceNormal ms idt tps exs bod

interfaceBodyParser :: (Parsable l) => P (InterfaceBody l)
interfaceBodyParser = (. catMaybes) <$> tP InterfaceBody <*>
    braces (list interfaceBodyDecl)

-- Declarations

classBodyStatement :: (Parsable l) => P (Maybe (DeclNode l))
classBodyStatement = tP $
    try (do
       _ <- list1 semiColon
       return const Nothing) <|>
    try ( do
       initDecl <-  InitDecl
       mst <- bopt (tok KW_Static)
       blk <- blockParser
       returnN $ \l ->  Just $ InitDecl l mst blk) <|>
    (do ms  <- list modifier
        dec <- memberDecl
        returnN $ \l -> Just $ MemberDecl l (dec ms))

memberDecl :: (Parsable l) => P (Mod l (MemberDeclN l))
memberDecl =
    try (do
        mDec <- tP MemberClassDecl
        cd <- classDeclParser
        return $ \ms l -> toNode $ MemberClassDecl l (cd ms))
    <|>
    try
        (do mInDec <- tP MemberInterfaceDecl
            idecl <- try annInterfaceDecl <|> try interfaceDeclParser
            return $ \ms l -> toNode $ MemberInterfaceDecl l $ idecl ms) <|>
    try (wrapM fieldDecl) <|>
    try (wrapM methodDecl) <|>
    constrDecl

fieldDecl :: (Parsable l) => P (Mod l (FieldDecl l))
fieldDecl = endSemi $ do
    typ <- ttype
    vds <- varDecls
    return $ \ms l -> FieldDecl l ms typ vds

methodDecl :: (Parsable l) => P (Mod l (MethodDecl l))
methodDecl = do
    tps <- lopt typeParams
    rt  <- resultType
    idt  <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- methodBodyParser
    return $ \ms l -> MethodDecl l ms tps rt idt fps thr Nothing bod

methodBodyParser :: (Parsable l) => P (MethodBody l)
methodBodyParser = MethodBody <$$>
    (const Nothing <$> semiColon <|> Just <$> blockParser)


constrDecl :: (Parsable l) => P (Mod l (ConstructorDecl l))
constrDecl = do
    tps <- lopt typeParams
    idt  <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- constrBodyParser
    return $ \ms l -> ConstructorDecl l ms tps idt fps thr bod

constrBodyParser :: (Parsable l) => P (ConstructorBody l)
constrBodyParser = tP $ braces $ do
    mec <- opt (try explConstrInv)
    bss <- list blockStmt
    return $ \l -> ConstructorBody mec bss

explConstrInv :: (Parsable l) => P (ExplConstrInv l)
explConstrInv = wrapP $ endSemi $
    try ( do
        tas <- lopt refTypeArgs
        tok KW_This
        as  <- args
        return $ \l -> ThisInvoke l tas as) <|>
    try ( do
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ \l -> SuperInvoke l tas as) <|>
    (do pri <- primaryParser
        period
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ \l -> PrimarySuperInvoke l pri tas as)

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDecl :: (Parsable l) => P (Maybe (MemberDeclNode l))
interfaceBodyDecl = semiColon >> return Nothing <|>
    tP $ do
        ms  <- list modifier
        imd <- interfaceMemberDecl
        return $ \l -> Just § imd ms l

interfaceMemberDecl :: (Parsable l) => P (Mod l (MemberDeclNode l))
interfaceMemberDecl =
    (do cd  <- classDeclParser
        return $ toNode $ \ms l -> MemberClassDecl l (cd ms)) <|>
    (do idt  <- try annInterfaceDecl <|> try interfaceDeclParser
        return $ toNode $ \ms l -> MemberInterfaceDecl l (idt ms)) <|>
    try fieldDecl <|>
    wrapM absMethodDecl

absMethodDecl :: (Parsable l) => P (Mod l (MethodDecl l))
absMethodDecl = do
    tps <- lopt typeParams
    rt  <- resultType
    idt  <- ident
    fps <- formalParams
    thr <- lopt throws
    def <- opt defaultValue
    body <- try methodBodyParser <|> tP $ semiColon >> return (flip MethodBody Nothing)
    return $ \ms l -> MethodDecl l ms tps rt idt fps thr def body

throws :: (Parsable l) => P [ExceptionType l]
throws = (tok KW_Throws >> refTypeList) >>= mapM (\x -> tP ExceptionType <*> pure x)

defaultValue :: (Parsable l) => P (ExpNode l)
defaultValue = tok KW_Default >> expParser

-- Formal parameters

formalParams :: (Parsable l) => P [FormalParam l]
formalParams = parens $ do
    fps <- seplist formalParam comma
    if validateFPs fps
     then return fps
     else fail "Only the last formal parameter may be of variable arity"
  where validateFPs :: [FormalParam l] -> Bool
        validateFPs [] = True
        validateFPs [_] = True
        validateFPs (FormalParam _ _ _ b _ :_) = not b

formalParam :: (Parsable l) => P (FormalParam l)
formalParam = do
    fP <- tP FormalParam
    ms  <- list modifier
    typ <- ttype
    var <- bopt ellipsis
    vid <- varDeclId
    return $ fP ms typ var vid

ellipsis :: P ()
ellipsis = period >> period >> period

-- Modifiers

modifier :: (Parsable l) => P (Modifier l)
modifier =
        tok KW_Public      >> tP Public
    <|> tok KW_Protected   >> tP Protected
    <|> tok KW_Private     >> tP Private
    <|> tok KW_Abstract    >> tP Abstract
    <|> tok KW_Static      >> tP Static
    <|> tok KW_Strictfp    >> tP StrictFP
    <|> tok KW_Final       >> tP Final
    <|> tok KW_Native      >> tP Native
    <|> tok KW_Transient   >> tP Transient
    <|> tok KW_Volatile    >> tP Volatile
    <|> tok KW_Synchronized >> tP Synchronized_
    <|> tok KW_Default >> tP DefaultModifier
    <|> Annotation <$$> annotationParser

annotationParser :: (Parsable l) => P (Annotation l)
annotationParser = flip ($) <$ tok Op_AtSign <*> name <*> (
               try (flip NormalAnnotation <$> parens evlist)
           <|> try (flip SingleElementAnnotation <$> parens elementValue)
           <|> try (MarkerAnnotation <$ return ())
        )

evlist :: (Parsable l) => P [(Ident, ElementValue l)]
evlist = seplist1 elementValuePair comma

elementValuePair :: (Parsable l) => P (Ident, ElementValue l)
elementValuePair = (,) <$> ident <* tok Op_Equal <*> elementValue

elementValue :: (Parsable l) => P (ElementValue l)
elementValue =
    EVVal <$$> (    InitArray <$$> arrayInit
               <|> InitExp    <$$> condExp )
    <|> EVAnn <$$> annotationParser


----------------------------------------------------------------------------
-- Variable declarations

varDecls :: (Parsable l) => P [VarDecl l]
varDecls = seplist1 varDecl comma

varDecl :: (Parsable l) => P (VarDecl l)
varDecl = do
    varDeclP <- tP VarDecl
    vid <- varDeclId
    mvi <- opt $ tok Op_Equal >> varInitParser
    return $ varDeclP vid mvi

varDeclId :: (Parsable l) => P (VarDeclArray l)
varDeclId = do
    varDec <- tP VarDeclArray
    varId <- tP VarId
    idt  <- ident
    abrkts <- list arrBrackets
    return $ foldl (\f _ -> varDec . f) varId abrkts idt

arrBrackets :: P ()
arrBrackets = brackets $ return ()

localVarDecl :: (Parsable l) => P ([Modifier l], Type, [VarDecl l])
localVarDecl = do
    ms  <- list modifier
    typ <- ttype
    vds <- varDecls
    return (ms, typ, vds)

varInitParser :: (Parsable l) => P (VarInitNode l)
varInitParser =
    InitArray <$$> arrayInit <|>
    InitExp   <$$> expParser

arrayInit :: (Parsable l) => P (ArrayInit l)
arrayInit = tP $ braces $ do
    vis <- seplist varInitParser comma
    _ <- opt comma
    return $ \l -> ArrayInit l vis

----------------------------------------------------------------------------
-- Statements

blockParser :: (Parsable l) => P (Block l)
blockParser = braces $ Block <$$> list blockStmt

blockStmt :: (Parsable l) => P (BlockStmtNode l)
blockStmt = tP $
    try ( do
        ms  <- list modifier
        cd  <- classDeclParser
        return $ \l -> ArrayInit l (cd ms)) <|>
    try ( do
        (m,t,vds) <- endSemi localVarDecl
        return $ \l -> LocalVars m t vds) <|>
    wrap' stmt

stmt :: (Parsable l) => P (l -> Stmt l)
stmt = ifStmt <|> whileStmt <|> forStmt <|> labeledStmtParser <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e  <- parens expParser
        th <- stmtNSI
        el <- optionMaybe $ tok KW_Else >> stmt
        return $ \l -> IfThenElse l e th el
    whileStmt = do
        tok KW_While
        e   <- parens expParser
        s   <- stmt
        return $ \l -> While l e s
    forStmt = do
        tok KW_For
        f <- parens $
            try ( do
                fi <- opt forInitParser
                semiColon
                e  <- opt expParser
                semiColon
                fu <- opt forUp
                return $ \s l -> BasicFor l fi e fu s) <|>
            (do ms <- list modifier
                t  <- ttype
                i  <- ident
                colon
                e  <- expParser
                return $ \s l -> EnhancedFor l ms t i e s)
        s <- stmt
        return $ f s
    labeledStmtParser = try $ do
        lbl <- ident
        colon
        s   <- stmt
        return $ \l -> Labeled l lbl s

stmtNSI :: (Parsable l) => P (Stmt l)
stmtNSI = tP $ ifStmt <|> whileStmt <|> forStmt <|> labeledStmtParser <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e  <- parens expParser
        th <- stmtNSI
        el <- optionMaybe $ tok KW_Else >> stmtNSI
        return $ \l -> IfThenElse l e th el
    whileStmt = do
        tok KW_While
        e <- parens expParser
        s <- stmtNSI
        return $ \l -> While l e s
    forStmt = do
        tok KW_For
        f <- parens $ try ( do
                fi <- opt forInitParser
                semiColon
                e  <- opt expParser
                semiColon
                fu <- opt forUp
                return $ \s l -> BasicFor l fi e fu s)
            <|> (do
                ms <- list modifier
                t  <- ttype
                i  <- ident
                colon
                e  <- expParser
                return $ \s l -> EnhancedFor l ms t i e s)
        s <- stmtNSI
        return $ f s
    labeledStmtParser = try $ do
        i <- ident
        colon
        s <- stmtNSI
        return $ \l -> Labeled i s

stmtNoTrail :: (Parsable l) => P (Stmt l)
stmtNoTrail =
    -- empty statement
    const <$> tP Empty <*> semiColon <|>
    -- inner blockParser
    StmtBlock <$$> blockParser <|>
    -- assertions
    endSemi ( do
        assert <- tP Assert
        tok KW_Assert
        e   <- expParser
        me2 <- opt $ colon >> expParser
        return $ assert e me2) <|>
    -- switch stmts
    (do switch <- tP Switch
        tok KW_Switch
        e  <- parens expParser
        sb <- switchBlock
        return $ switch e sb) <|>
    -- do-while loops
    endSemi (do
        doNode <- tP Do
        tok KW_Do
        s <- stmt
        tok KW_While
        e <- parens expParser
        return $ doNode s e) <|>
    -- break
    endSemi (do
        brk <- tP Break
        tok KW_Break
        mi <- opt ident
        return $ brk mi) <|>
    -- continue
    endSemi (do
        cont <- tP Continue
        tok KW_Continue
        mi <- opt ident
        return $ cont mi) <|>
    -- return
    endSemi (do
        ret <- tP Return
        tok KW_Return
        me <- opt expParser
        return $ ret me) <|>
    -- synchronized
    (do sync <- tP Synchronized
        tok KW_Synchronized
        e <- parens expParser
        b <- blockParser
        return $ sync e b) <|>
    -- throw
    endSemi (do
        thr <- tP Throw
        tok KW_Throw
        e <- expParser
        return $ thr e) <|>
    -- try-catch, both with and without a finally clause
    (do tryNode <- tP Try
        tok KW_Try
        res <- fromMaybe [] <$> optionMaybe (parens tryResources)
        b <- blockParser
        c <- list catch
        mf <- opt $ tok KW_Finally >> blockParser
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        return $ tryNode res b c mf) <|>
    -- expressions as stmts
    ExpStmt <$$> endSemi stmtExp

-- For loops

forInitParser :: (Parsable l) => P (ForInit l)
forInitParser = try (do forLocalVars <- tP ForLocalVars
                        (m,t,vds) <- localVarDecl
                        return $ forLocalVars m t vds) <|>
    (tP ForInitExps <*> seplist1 stmtExp comma)

forUp :: (Parsable l) => P [Exp l]
forUp = seplist1 stmtExp comma

-- Switches

switchBlock :: (Parsable l) => P [SwitchBlock l]
switchBlock = braces $ list switchStmt

switchStmt :: (Parsable l) => P (SwitchBlock l)
switchStmt = do
    switch <- tP SwitchBlock
    lbl <- switchLabelParser
    bss <- list blockStmt
    return $ switch lbl bss

switchLabelParser :: (Parsable l) => P (SwitchLabel l)
switchLabelParser = (tok KW_Default >> colon >> tP Default) <|>
    (do switch <- tP SwitchCase
        tok KW_Case
        e <- expParser
        colon
        return $ switch e)

-- Try-catch clauses

tryResources :: (Parsable l) => P [TryResource l]
tryResources = seplist tryRes semiColon
    where
      tryRes = try (do
            tryResourceVar <- tP TryResourceVar
            mods <- list modifier
            ty <- refType
            vars <- varDecls
            return $ tryResourceVar mods ty vars
        ) <|> (tP TryResourceFinalVar <*> ident)

catch :: (Parsable l) => P (Catch l)
catch = do
    catchP <- tP Catch
    tok KW_Catch
    fp <- parens formalParam
    b  <- blockParser
    return $ catchP fp b

----------------------------------------------------------------------------
-- Expressions

stmtExp :: (Parsable l) => P (Exp l)
stmtExp = try preIncDec
    <|> try postIncDec
    <|> try assignment
    -- There are sharing gains to be made by unifying these two
    <|> try methodInvocationExp
    <|> try lambdaExp
    <|> try methodRef
    <|> instanceCreation

preIncDec :: (Parsable l) => P (Exp l)
preIncDec = do
    op <- preIncDecOp
    e <- unaryExp
    return $ op e

postIncDec :: (Parsable l) => P (Exp l)
postIncDec = do
    e <- postfixExpNES
    ops <- list1 postfixOp
    return $ foldl (\a s -> s a) e ops

assignment :: (Parsable l) => P (Exp l)
assignment = do
    assign <- tP Assign
    lh <- lhs
    op <- assignOpParser
    e  <- assignExp
    return $ assign lh op e

lhs :: (Parsable l) => P (Lhs l)
lhs = try (FieldLhs <$$> fieldAccessParser)
    <|> try (ArrayLhs <$$> arrayAccess)
    <|> NameLhs <$$> name



expParser :: (Parsable l) => P (Exp l)
expParser = assignExp

assignExp :: (Parsable l) => P (Exp l)
assignExp = try methodRef <|> try lambdaExp <|> try assignment <|> condExp

condExp :: (Parsable l) => P (Exp l)
condExp = do
    ie <- infixExp
    ces <- list condExpSuffix
    return $ foldl (\a s -> s a) ie ces

condExpSuffix :: (Parsable l) => P (Exp l -> Exp l)
condExpSuffix = do
    cond <- tP Cond
    tok Op_Query
    th <- expParser
    colon
    el <- condExp
    return $ \ce -> cond ce th el

infixExp :: (Parsable l) => P (Exp l)
infixExp = infixExpWithOperators infixOperators

-- See Note [Parsing operators]
infixExpWithOperators :: (Parsable l) => [P Op] -> P (Exp l)
infixExpWithOperators [] = unaryExp
infixExpWithOperators (op : ops) = do
    ue <- infixExpWithOperators ops
    ies <- list (infixExpSuffix op ops)
    return $ foldl (\a s -> s a) ue ies

infixExpSuffix :: (Parsable l) => P Op -> [P Op] -> P (Exp l -> Exp l)
infixExpSuffix infixOp ops =
    (do binOpP <- tP BinOp
        op <- infixOp
        e2 <- infixExpWithOperators ops
        return $ \e1 -> binOpP e1 op e2) <|>

    -- FIXME 'instanceof' should have the same precedence as relational operators
    (do insOf <- tP InstanceOf
        tok KW_Instanceof
        t  <- refType
        return $ \e1 -> insOf e1 t)

unaryExp :: (Parsable l) => P (Exp l)
unaryExp = try preIncDec <|>
    try (do
        op <- prefixOp
        ue <- unaryExp
        return $ op ue) <|>
    try (do
        cast <- tP Cast
        t <- parens ttype
        e <- unaryExp
        return $ cast t e) <|>
    postfixExp

postfixExpNES :: (Parsable l) => P (Exp l)
postfixExpNES = primaryParser

postfixExp :: (Parsable l) => P (Exp l)
postfixExp = do
    pe <- postfixExpNES
    ops <- list postfixOp
    return $ foldl (\a s -> s a) pe ops


primaryParser :: (Parsable l) => P (Exp l)
primaryParser = primaryNPS |>> primarySuffix

primaryNPS :: (Parsable l) => P (Exp l)
primaryNPS = try arrayCreation <|> primaryNoNewArrayNPS

-- primaryNoNewArray = startSuff primaryNoNewArrayNPS primarySuffix

primaryNoNewArrayNPS :: (Parsable l) => P (Exp l)
primaryNoNewArrayNPS =
    Lit <$$> literalParser <|>
    const <$> tP This <*> tok KW_This <|>
    parens expParser <|>
    -- TODO: These two following should probably be merged more
    try ( do
        classLitP <- tP ClassLit
        rt <- resultType
        period >> tok KW_Class
        return $ classLitP rt) <|>
    try ( do
        qualThis <- tP QualifiedThis
        t <- ttype
        period >> tok KW_This
        return $ qualThis t) <|>
    try instanceCreationNPS <|>
    try (MethodInv <$$> methodInvocationNPS) <|>
    try (FieldAccess <$$> fieldAccessNPS) <|>
    try (ExpName <$$> name) <|>
    ArrayAccess <$$> arrayAccessNPS

primarySuffix :: (Parsable l) => P (Exp l -> Exp l)
primarySuffix = try instanceCreationSuffix <|>
    try ((.) <$> tP ArrayAccess <*> arrayAccessSuffix) <|>
    try ((.) <$> tP MethodInv <*> methodInvocationSuffix) <|>
    (.) <$> tP FieldAccess <*> fieldAccessSuffix


instanceCreationNPS :: (Parsable l) => P (Exp l)
instanceCreationNPS =
    do instCr <- tP InstanceCreation
       tok KW_New
       tas <- lopt typeArgsParser
       tds <- typeDeclSpecifier
       as  <- args
       mcb <- opt classBodyParser
       return $ instCr tas tds as mcb

typeDeclSpecifier :: P TypeDeclSpecifier
typeDeclSpecifier =
    do ct <- classType
       return $ TypeDeclSpecifier ct


instanceCreationSuffix :: (Parsable l) => P (Exp l -> Exp l)
instanceCreationSuffix =
     do qualCre <- tP QualInstanceCreation
        period >> tok KW_New
        tas <- lopt typeArgsParser
        i   <- ident
        as  <- args
        mcb <- opt classBodyParser
        return $ \p -> qualCre p tas i as mcb

instanceCreation :: (Parsable l) => P (Exp l)
instanceCreation = try instanceCreationNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let icp = foldl (\a s -> s a) p ss
    case icp of
     QualInstanceCreation {} -> return icp
     _ -> fail ""


lambdaParamsParser :: (Parsable l) => P (LambdaParams l)
lambdaParamsParser = try (LambdaSingleParam <$$> ident)
               <|> try (parens $ LambdaFormalParams <$$> seplist formalParam comma)
               <|> parens (LambdaInferredParams <$$> seplist ident comma)

lambdaExp :: (Parsable l) => P (Exp l)
lambdaExp = Lambda <$$> (lambdaParamsParser <* tok LambdaArrow)
            <*> ((LambdaBlock <$$> try blockParser)
                 <|> (LambdaExpression <$$> expParser))

methodRef :: (Parsable l) => P (Exp l)
methodRef = MethodRef <$$> (name <*  tok MethodRefSep)
            <*> ident

{-
instanceCreation =
    (do tok KW_New
        tas <- lopt typeArgsParser
        ct  <- classType
        as  <- args
        mcb <- opt classBodyParser
        return $ InstanceCreation tas ct as mcb) <|>
    (do p   <- primaryParser
        period >> tok KW_New
        tas <- lopt typeArgsParser
        i   <- ident
        as  <- args
        mcb <- opt classBodyParser
        return $ QualInstanceCreation p tas i as mcb)
-}

fieldAccessNPS :: (Parsable l) => P (FieldAccess l)
fieldAccessNPS =
    (do sFieldAcc <- tP SuperFieldAccess
        tok KW_Super >> period
        i <- ident
        return $ sFieldAcc i) <|>
    (do clFieldAcc <- tP ClassFieldAccess
        n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ clFieldAcc n i)

fieldAccessSuffix :: (Parsable l) => P (Exp l -> FieldAccess l)
fieldAccessSuffix = do
    primFAcc <- tP PrimaryFieldAccess
    period
    i <- ident
    return $ \p -> primFAcc p i

fieldAccessParser :: (Parsable l) => P (FieldAccess l)
fieldAccessParser = try fieldAccessNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let fap = foldl (\a s -> s a) p ss
    case fap of
     FieldAccess _ fa -> return fa
     _ -> fail ""

{-
fieldAccessParser :: P FieldAccess
fieldAccessParser = try fieldAccessNPS <|> do
    p <- primaryParser
    fs <- fieldAccessSuffix
    return (fs p)
-}

{-
fieldAccessParser :: P FieldAccess
fieldAccessParser =
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccess i) <|>
    (try $ do
        n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess n i) <|>
    (do p <- primaryParser
        period
        i <- ident
        return $ PrimaryFieldAccess p i)
-}

methodInvocationNPS :: (Parsable l) => P (MethodInvocation l)
methodInvocationNPS =
    (do sMCall <- tP SuperMethodCall
        tok KW_Super >> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ sMCall rts i as) <|>
    (do n <- name
        f <- (do mCall <- tP MethodCall
                 as <- args
                 return $ \na -> mCall na as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                tyMethCall <- tP TypeMethodCall
                clMethCall <- tP ClassMethodCall
                let mc = maybe tyMethCall (const clMethCall) msp
                return $ \na -> mc na rts i as)
        return $ f n)

methodInvocationSuffix :: (Parsable l) => P (Exp l -> MethodInvocation l)
methodInvocationSuffix = do
        primMethCall <- tP PrimaryMethodCall
        period
        _ <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ \p -> primMethCall p [] i as

methodInvocationExp :: (Parsable l) => P (Exp l)
methodInvocationExp = try (do
    p <- primaryNPS
    ss <- list primarySuffix
    let mip = foldl (\a s -> s a) p ss
    case mip of
     MethodInv _ _ -> return mip
     _ -> fail "") <|>
     (MethodInv <$$> methodInvocationNPS)

{-
methodInvocation :: P MethodInvocation
methodInvocation =
    (do tok KW_Super >> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ SuperMethodCall rts i as) <|>
    (do p <- primaryParser
        period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ PrimaryMethodCall p rts i as) <|>
    (do n <- name
        f <- (do as <- args
                 return $ \n -> MethodCall n as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                return $ \n -> mc n rts i as)
        return $ f n)
-}

args :: (Parsable l) => P [Argument l]
args = parens $ seplist expParser comma

-- Arrays

arrayAccessNPS :: (Parsable l) => P (ArrayIndex l)
arrayAccessNPS = do
    arrInd <- tP ArrayIndex
    exName <- tP ExpName
    n <- name
    e <- list1 $ brackets expParser
    return $ arrInd (exName n) e

arrayAccessSuffix :: (Parsable l) => P (Exp l -> ArrayIndex l)
arrayAccessSuffix = do
    arrInd <- tP ArrayIndex
    e <- list1 $ brackets expParser
    return $ \ref -> arrInd ref e

arrayAccess :: (Parsable l) => P (ArrayIndex l)
arrayAccess = try arrayAccessNPS <|> do
    p <- primaryNoNewArrayNPS
    ss <- list primarySuffix
    let aap = foldl (\a s -> s a) p ss
    case aap of
     (ArrayAccess _ ain) -> return ain
     _ -> fail ""

{-
arrayAccess :: P (Exp, Exp)
arrayAccess = do
    ref <- arrayRef
    e   <- brackets expParser
    return (ref, e)

arrayRef :: P Exp
arrayRef = ExpName <$> name <|> primaryNoNewArray
-}

arrayCreation :: (Parsable l) => P (Exp l)
arrayCreation = do
    tok KW_New
    t <- nonArrayType
    f <- try (do
             arrCrI <- tP ArrayCreateInit
             ds <- list1 $ brackets empty
             ai <- arrayInit
             return $ \tt -> arrCrI tt (length ds) ai) <|>
         (do arrCr <- tP ArrayCreate
             des <- list1 $ try $ brackets expParser
             ds  <- list  $ brackets empty
             return $ \tt -> arrCr tt des (length ds))
    return $ f t

literalParser :: P Literal
literalParser =
    javaToken $ \t -> case t of
        IntTok     i -> Just (Int i)
        LongTok    l -> Just (Word l)
        DoubleTok  d -> Just (Double d)
        FloatTok   f -> Just (Float f)
        CharTok    c -> Just (Char c)
        StringTok  s -> Just (String s)
        BoolTok    b -> Just (Boolean b)
        NullTok      -> Just Null
        _ -> Nothing

-- Operators

preIncDecOp, prefixOp, postfixOp :: (Parsable l) => P (Exp l -> Exp l)
preIncDecOp =
    (tok Op_PPlus >> tP PreIncrement) <|>
    (tok Op_MMinus >> tP PreDecrement)
prefixOp =
    (tok Op_Bang  >> tP PreNot      ) <|>
    (tok Op_Tilde >> tP PreBitCompl ) <|>
    (tok Op_Plus  >> tP PrePlus     ) <|>
    (tok Op_Minus >> tP PreMinus    )
postfixOp =
    (tok Op_PPlus  >> tP PostIncrement) <|>
    (tok Op_MMinus >> tP PostDecrement)

assignOpParser :: P AssignOp
assignOpParser =
    (tok Op_Equal    >> return EqualA   ) <|>
    (tok Op_StarE    >> return MultA    ) <|>
    (tok Op_SlashE   >> return DivA     ) <|>
    (tok Op_PercentE >> return RemA     ) <|>
    (tok Op_PlusE    >> return AddA     ) <|>
    (tok Op_MinusE   >> return SubA     ) <|>
    (tok Op_LShiftE  >> return LShiftA  ) <|>
    (tok Op_RShiftE  >> return RShiftA  ) <|>
    (tok Op_RRShiftE >> return RRShiftA ) <|>
    (tok Op_AndE     >> return AndA     ) <|>
    (tok Op_CaretE   >> return XorA     ) <|>
    (tok Op_OrE      >> return OrA      )

-- The infix operators, grouped by precedence.
-- See Note [Parsing operators]
infixOperators :: [P Op]
infixOperators =
  [ tok Op_OOr     >> return COr

  , tok Op_AAnd    >> return CAnd

  , tok Op_Or      >> return Or

  , tok Op_Caret   >> return Xor

  , tok Op_And     >> return And

  , (tok Op_Equals  >> return Equal     ) <|>
    (tok Op_BangE   >> return NotEq     )

  , (tok Op_LThan   >> return LThan     ) <|>
    (tok Op_GThan   >> return GThan     ) <|>
    (tok Op_LThanE  >> return LThanE    ) <|>
    (tok Op_GThanE  >> return GThanE    )

  , (tok Op_LShift  >> return LShift    ) <|>
    try (do
       tok Op_GThan
       tok Op_GThan
       tok Op_GThan
       return RRShift ) <|>

    try (do
       tok Op_GThan
       tok Op_GThan
       return RShift )

  , (tok Op_Plus    >> return Add       ) <|>
    (tok Op_Minus   >> return Sub       )

  , (tok Op_Star    >> return Mult      ) <|>
    (tok Op_Slash   >> return Div       ) <|>
    (tok Op_Percent >> return Rem       )
  ]

{-
Note [Parsing operators]
~~~~~~~~~~~~~~~~~~~~~~~~
Each entry in 'infixOperators' generates one level of recursion in
'infixExpWithOperators'. This generates a grammar similar to:

@
  ConditionalOrExpression ::=
    ConditionalAndExpression [ "||" ConditionalOrExpression ]

  ConditionalAndExpression ::=
    InclusiveOrExpression [ "&&" ConditionalAndExpression ]

  // and so on...
@

but the operators associate to the left.

A similar (but more general) pattern can be found in 'Text.Parsec.Expr'.
-}

----------------------------------------------------------------------------
-- Types

ttype :: P Type
ttype = try (RefType <$> refType) <|> PrimType <$> primType

primType :: P PrimType
primType =
    tok KW_Boolean >> return BooleanT  <|>
    tok KW_Byte    >> return ByteT     <|>
    tok KW_Short   >> return ShortT    <|>
    tok KW_Int     >> return IntT      <|>
    tok KW_Long    >> return LongT     <|>
    tok KW_Char    >> return CharT     <|>
    tok KW_Float   >> return FloatT    <|>
    tok KW_Double  >> return DoubleT

refType :: P RefType
refType =
    (do pt <- primType
        (_:bs) <- list1 arrBrackets
        return $ foldl (\f _ -> ArrayType . RefType . f)
                        (ArrayType . PrimType) bs pt) <|>
    (do ct <- classType
        bs <- list arrBrackets
        return $ foldl (\f _ -> ArrayType . RefType . f)
                            ClassRefType bs ct) <?> "refType"

nonArrayType :: P Type
nonArrayType = PrimType <$> primType <|>
    (RefType . ClassRefType <$> classType)

classType :: P ClassType
classType = toClassType <$> parseClass
    where
        toClassType :: [(Ident, [TypeArgument])] -> ClassType
        toClassType = constructClassType . stripEmpty . split
        split = span (\(idt, _) -> maybe False (\a -> a == toLower a) ((listToMaybe . fromIdent) idt))
        stripEmpty (a, b) = (map fst a, b)
        constructClassType (a, b) = if null a
                then WithoutPackage (ClassName b)
                else WithPackage (FullQualiPackage a) (ClassName b)

        parseClass = sepListEndOptBy (classTypeSpec typeArgsParser) (classTypeSpec typeArgsWithDiamond) period

        typeArgsWithDiamond = try typeArgsParser <|> (:[]) <$> typeDiamond

classTypeSpec :: P [TypeArgument] -> P (Ident, [TypeArgument])
classTypeSpec argsP = do
    i   <- ident
    tas <- lopt argsP
    return (i, tas)

resultType :: P (Maybe Type)
resultType = tok KW_Void >> return Nothing <|> Just <$> ttype <?> "resultType"

refTypeList :: P [RefType]
refTypeList = seplist1 refType comma

----------------------------------------------------------------------------
-- Type parameters and arguments

typeParams :: P [TypeParam]
typeParams = angles $ seplist1 typeParam comma

typeParam :: P TypeParam
typeParam = do
    i  <- ident
    bs <- lopt bounds
    return $ TypeParam i bs

bounds :: P [RefType]
bounds = tok KW_Extends >> seplist1 refType (tok Op_And)

typeArgsParser :: P [TypeArgument]
typeArgsParser = angles $ seplist1 typeArg comma

typeArg :: P TypeArgument
typeArg = tok Op_Query >> Wildcard <$> opt wildcardBound
    <|> ActualType <$> refType

typeDiamond :: P TypeArgument
typeDiamond = angles $ pure Diamond

wildcardBound :: P WildcardBound
wildcardBound = tok KW_Extends >> ExtendsBound <$> refType
    <|> tok KW_Super >> SuperBound <$> refType

refTypeArgs :: P [RefType]
refTypeArgs = angles refTypeList

----------------------------------------------------------------------------
-- Names

name :: P Name
name = Name <$> seplist1 ident period

ident :: P Ident
ident = javaToken $ \t -> case t of
    IdentTok s -> Just $ Ident s
    _ -> Nothing

----------------------------------------------------------------------------
-- Package

fullQualiPkg :: P Package
fullQualiPkg = FullQualiPackage <$> seplist1 ident period

wildcardPkg :: P Package
wildcardPkg = WildcardPackage <$> seplist1 ident period
------------------------------------------------------------

empty :: P ()
empty = return ()

opt :: P a -> P (Maybe a)
opt = optionMaybe

bopt :: P a -> P Bool
bopt p = opt p >>= \ma -> return $ isJust ma

lopt :: P [a] -> P [a]
lopt p = do mas <- opt p
            case mas of
             Nothing -> return []
             Just as -> return as

list :: P a -> P [a]
list = option [] . list1

list1 :: P a -> P [a]
list1 = many1

seplist :: P a -> P sep -> P [a]
--seplist = sepBy
seplist p sep = option [] $ seplist1 p sep

seplist1 :: P a -> P sep -> P [a]
--seplist1 = sepBy1
seplist1 p sep =
    p >>= \a ->
        try (do _ <- sep
                as <- seplist1 p sep
                return (a:as))
        <|> return [a]

sepListEndOptBy :: P a -> P a -> P sep -> P [a]
sepListEndOptBy p end sep =
        try (p >>= \a ->
            try (do _ <- sep
                    as <- sepListEndOptBy p end sep
                    return (a:as))
            <|> return [a])
        <|> ((:[]) <$> end)


startSuff, (|>>) :: P a -> P (a -> a) -> P a
startSuff start suffix = do
    x <- start
    ss <- list suffix
    return $ foldl (\a s -> s a) x ss

(|>>) = startSuff

------------------------------------------------------------

javaToken :: (Token -> Maybe a) -> P a
javaToken testTok = token showT posT testT
  where showT (L _ t) = show t
        posT  (L p _) = pos2sourcePos p
        testT (L _ t) = testTok t

tok, matchToken :: Token -> P ()
tok = matchToken
matchToken t = javaToken (\r -> if r == t then Just () else Nothing)

pos2sourcePos :: (Int, Int) -> SourcePos
pos2sourcePos (l,c) = newPos "" l c

type Mod l a = [Modifier l] -> l -> a

parens, braces, brackets, angles :: P a -> P a
parens   = between (tok OpenParen)  (tok CloseParen)
braces   = between (tok OpenCurly)  (tok CloseCurly)
brackets = between (tok OpenSquare) (tok CloseSquare)
angles   = between (tok Op_LThan)   (tok Op_GThan)

endSemi :: P a -> P a
endSemi p = p >>= \a -> semiColon >> return a

comma, colon, semiColon, period :: P ()
comma     = tok Comma
colon     = tok Op_Colon
semiColon = tok SemiColon
period    = tok Period

------------------------------------------------------------

test = "public class Foo { }"
testFile file = do
  i <- readFile file
  let r = parseCompilationUnit i
  putStrLn$ either (("Parsing error:\n"++) . show) (show . pretty) r

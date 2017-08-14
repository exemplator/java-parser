{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
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
import           Control.Monad               (foldM)

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
(<$$>) constr pa = (flip constr) <$> pa
infixl 4 <$$>

(<**>) :: (Parsable l) => P (l -> a -> b) -> P a -> P (l -> b)
(<**>) pconstr pa = flip <$> pconstr <*> pa
infixl 4 <**>

returnN :: (Monad m, HasNode a b) => (l -> a l) -> m (l -> b l)
returnN a = return $ \l -> toNode (a l)

returnN' :: (Monad m, HasNode a b) => a l -> m (b l)
returnN' a = return $ toNode a

wrap :: (Applicative f, HasNode a b) => f (a l) -> f (b l)
wrap = fmap toNode

wrapP :: (Parsable l, HasNode a b) => P (l -> a l) -> P (b l)
wrapP = tP . wrapL

wrapL :: (Applicative f, HasNode a b) => f (l -> (a l)) -> f (l -> (b l))
wrapL a = (\a l -> toNode (a l)) <$> a

wrapM :: (Applicative f, HasNode a b) => f (Mod l (a l)) -> f (Mod l (b l))
wrapM a = (\a ms l -> toNode (a ms l)) <$> a

wrapE :: (Applicative f, HasNode a b) => f (Exp l (a l)) -> f (Exp l (b l))
wrapE a = (\a e l -> toNode (a e l)) <$> a

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

compilationUnit :: Parsable l => P (CompilationUnit l)
compilationUnit = tP $ do
    mpd <- opt packageDeclParser
    ids <- list importDecl
    tds <- list typeDeclParser
    eof
    return $ \l -> CompilationUnit l mpd ids (catMaybes tds)

moduleDeclaration :: Parsable l => P (ModuleDeclaration l)
moduleDeclaration = tP $ do
    -- only tokens in module descriptions!
    (Ident "module") <- ident
    modulePackageP <- fullQualiPkg
    moduleSpecsP <- braces $ list moduleSpecParser
    return $ \l -> ModuleDeclaration l modulePackageP moduleSpecsP

packageDeclParser :: (Parsable l) => P (PackageDecl l)
packageDeclParser = tP $ do
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
classOrInterfaceDecl = tP $ do
    ms <- list modifier
    de <- (wrapM classDeclParser) <|>
          wrapM annInterfaceDecl <|> wrapM interfaceDeclParser
    return $ de ms

classDeclParser :: (Parsable l) => P (Mod l (ClassDeclNode l))
classDeclParser = wrapM normalClassDecl <|> wrapM enumClassDecl

normalClassDecl :: (Parsable l) => P (Mod l (ClassDecl l))
normalClassDecl = do
    tok KW_Class
    i   <- ident
    tps <- lopt typeParams
    mex <- opt extendsParser
    imp <- lopt implementsParsre
    bod <- classBodyParser
    return $ \ms l -> ClassDecl l ms i tps (fmap head mex) imp bod

extendsParser :: (Parsable l) => P [Extends l]
extendsParser = tok KW_Extends >> (map (uncurry Extends)) <$> refTypeList

implementsParsre :: (Parsable l) => P [Implements l]
implementsParsre = tok KW_Implements >> (map (uncurry Implements)) <$> refTypeList

enumClassDecl :: (Parsable l) => P (Mod l (EnumDecl l))
enumClassDecl = do
    tok KW_Enum
    i   <- ident
    imp <- lopt implementsParsre
    bod <- enumBodyParser
    return $ \ms l -> EnumDecl l ms i imp bod

classBodyParser :: (Parsable l) => P (ClassBody l)
classBodyParser = tP $ (flip ClassBody) <$> braces classBodyStatements

enumBodyParser :: (Parsable l) => P (EnumBody l)
enumBodyParser = tP $ braces $ do
    ecs <- seplist enumConst comma
    optional comma
    eds <- lopt enumBodyDecls
    return $ \l -> EnumBody l ecs eds

enumConst :: (Parsable l) => P (EnumConstant l)
enumConst = tP $ do
    idt  <- ident
    as  <- lopt args
    mcb <- opt classBodyParser
    return $ \l -> EnumConstant l idt as mcb

enumBodyDecls :: (Parsable l) => P [DeclNode l]
enumBodyDecls = semiColon >> classBodyStatements

classBodyStatements :: (Parsable l) => P [DeclNode l]
classBodyStatements = catMaybes <$> list classBodyStatement

-- Interface declarations

annInterfaceDecl :: (Parsable l) => P (Mod l (InterfaceDecl l))
annInterfaceDecl = do
    tok KW_AnnInterface
    idt  <- ident
    tps <- lopt typeParams
    exs <- lopt extendsParser
    bod <- interfaceBodyParser
    return $ \ms l -> InterfaceDecl l InterfaceAnnotation ms idt tps exs bod

interfaceDeclParser :: (Parsable l) => P (Mod l (InterfaceDecl l))
interfaceDeclParser = do
    tok KW_Interface
    idt  <- ident
    tps <- lopt typeParams
    exs <- lopt extendsParser
    bod <- interfaceBodyParser
    return $ \ms l -> InterfaceDecl l InterfaceNormal ms idt tps exs bod

interfaceBodyParser :: (Parsable l) => P (InterfaceBody l)
interfaceBodyParser = tP $ (\m l -> InterfaceBody l $ catMaybes m) <$> braces (list interfaceBodyDecl)

-- Declarations

classBodyStatement :: (Parsable l) => P (Maybe (DeclNode l))
classBodyStatement = tP $
    try (do
       _ <- list1 semiColon
       return $ const Nothing) <|>
    try ( do
       mst <- bopt (tok KW_Static)
       blk <- blockParser
       return $ \l ->  Just $ toNode $ InitDecl l mst blk) <|>
    (do ms  <- list modifier
        dec <- memberDecl
        return $ \l -> Just $ toNode $ dec ms l)

memberDecl :: (Parsable l) => P (Mod l (MemberDeclNode l))
memberDecl =
    try (wrapM classDeclParser) <|>
    try (wrapM $ annInterfaceDecl <|> try interfaceDeclParser) <|>
    try (wrapM fieldDecl) <|>
    try (wrapM methodDecl) <|>
    wrapM constrDecl

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
methodBodyParser = tP $ (flip MethodBody) <$>
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
    return $ \l -> ConstructorBody l mec bss

explConstrInv :: (Parsable l) => P (ExplConstrInvNode l)
explConstrInv = tP $ endSemi $
    try ( do
        tas <- (map snd) <$> lopt (refTypeArgs :: P [(l, RefType)])
        tok KW_This
        as  <- args
        returnN $ \l -> ThisInvoke l tas as) <|>
    try ( do
        tas <- (map snd) <$> lopt refTypeArgs
        tok KW_Super
        as  <- args
        returnN $ \l -> SuperInvoke l tas as) <|>
    (do pri <- primaryParser
        period
        tas <- (map snd) <$> lopt refTypeArgs
        tok KW_Super
        as  <- args
        returnN $ \l -> PrimarySuperInvoke l pri tas as)

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDecl :: (Parsable l) => P (Maybe (MemberDeclNode l))
interfaceBodyDecl = (semiColon >> return Nothing) <|>
    tP $ do
        ms  <- list modifier
        imd <- interfaceMemberDecl
        return $ \l -> Just $ imd ms l

interfaceMemberDecl :: (Parsable l) => P (Mod l (MemberDeclNode l))
interfaceMemberDecl =
    (wrapM classDeclParser) <|>
    (wrapM (try annInterfaceDecl <|> try interfaceDeclParser)) <|>
    try (wrapM fieldDecl) <|>
    wrapM absMethodDecl

absMethodDecl :: (Parsable l) => P (Mod l (MethodDecl l))
absMethodDecl = do
    tps <- lopt typeParams
    rt  <- resultType
    idt  <- ident
    fps <- formalParams
    thr <- lopt throws
    def <- opt defaultValue
    body <- try (methodBodyParser) <|> (wrapP $ semiColon >> return (flip MethodBody Nothing))
    return $ \ms l -> MethodDecl l ms tps rt idt fps thr def body

throws :: (Parsable l) => P [ExceptionType l]
throws = (map $ uncurry ExceptionType) <$> (tok KW_Throws >> refTypeList)

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
formalParam = tP $ do
    ms  <- list modifier
    typ <- ttype
    var <- bopt ellipsis
    vid <- varDeclId
    return $ \l -> FormalParam l ms typ var (toNode vid)

ellipsis :: P ()
ellipsis = period >> period >> period

-- Modifiers

modifier :: (Parsable l) => P (Modifier l)
modifier = tP (
        tok KW_Public      >> return Public
    <|> tok KW_Protected   >> return Protected
    <|> tok KW_Private     >> return Private
    <|> tok KW_Abstract    >> return Abstract
    <|> tok KW_Static      >> return Static
    <|> tok KW_Strictfp    >> return StrictFP
    <|> tok KW_Final       >> return Final
    <|> tok KW_Native      >> return Native
    <|> tok KW_Transient   >> return Transient
    <|> tok KW_Volatile    >> return Volatile
    <|> tok KW_Synchronized >> return Synchronized_
    <|> tok KW_Default >> return DefaultModifier )
    <|> wrap annotationParser

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
    tP $ (EVVal <$$> ( (wrap arrayInit)
               <|> wrap condExp ))
    <|> EVAnn <$$> annotationParser


----------------------------------------------------------------------------
-- Variable declarations

varDecls :: (Parsable l) => P [VarDecl l]
varDecls = seplist1 varDecl comma

varDecl :: (Parsable l) => P (VarDecl l)
varDecl = tP $ do
    vid <- wrap varDeclId
    mvi <- opt $ tok Op_Equal >> varInitParser
    return $ \l -> VarDecl l vid mvi

varDeclId :: (Parsable l) => P (VarDeclIdNode l)
varDeclId = do
    varId <- wrapP $ VarId <$$> ident
    brkts <- list arrBrackets
    return $ foldl (\f _ -> VarDeclArrayNode . f) varId brkts

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
    wrap arrayInit <|>
    wrap expParser

arrayInit :: (Parsable l) => P (ArrayInit l)
arrayInit = tP $ braces $ do
    vis <- seplist varInitParser comma
    _ <- opt comma
    return $ \l -> ArrayInit l vis

----------------------------------------------------------------------------
-- Statements

blockParser :: (Parsable l) => P (Block l)
blockParser = tP $ braces $ (flip Block) <$> list blockStmt

blockStmt :: (Parsable l) => P (BlockStmtNode l)
blockStmt = tP $
    try ( do
        ms  <- list modifier
        cd  <- classDeclParser
        returnN $ cd ms) <|>
    try ( do
        (m,t,vds) <- endSemi localVarDecl
        returnN $ \l -> LocalVars l m t vds) <|>
    (wrap stmt)

stmt :: (Parsable l) => P (StmtNode l)
stmt = tP (ifStmt <|> whileStmt <|> forStmt <|> labeledStmtParser) <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e  <- parens expParser
        th <- stmtNSI
        el <- optionMaybe $ tok KW_Else >> stmt
        returnN $ \l -> IfThenElse l e th el
    whileStmt = do
        tok KW_While
        e   <- parens expParser
        s   <- stmt
        returnN $ \l -> While l e s
    forStmt = do
        tok KW_For
        f <- parens $
            try ( do
                fi <- opt forInitParser
                semiColon
                e  <- opt expParser
                semiColon
                fu <- opt forUp
                return $ \s l -> toNode $ BasicFor l fi e fu s) <|>
            (do ms <- list modifier
                t  <- ttype
                i  <- ident
                colon
                e  <- expParser
                return $ \s l -> toNode $ EnhancedFor l ms t i e s)
        s <- stmt
        return $ f s
    labeledStmtParser = try $ do
        lbl <- ident
        colon
        s   <- stmt
        returnN $ \l -> Labeled l lbl s

stmtNSI :: (Parsable l) => P (StmtNode l)
stmtNSI = tP (ifStmt <|> whileStmt <|> forStmt <|> labeledStmtParser) <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e  <- parens expParser
        th <- stmtNSI
        el <- optionMaybe $ tok KW_Else >> stmtNSI
        returnN $ \l -> IfThenElse l e th el
    whileStmt = do
        tok KW_While
        e <- parens expParser
        s <- stmtNSI
        returnN $ \l -> While l e s
    forStmt = do
        tok KW_For
        f <- parens $ try ( do
                fi <- opt forInitParser
                semiColon
                e  <- opt expParser
                semiColon
                fu <- opt forUp
                return $ \s l -> toNode $ BasicFor l fi e fu s)
            <|> (do
                ms <- list modifier
                t  <- ttype
                i  <- ident
                colon
                e  <- expParser
                return $ \s l -> toNode $ EnhancedFor l ms t i e s)
        s <- stmtNSI
        return $ f s
    labeledStmtParser = try $ do
        i <- ident
        colon
        s <- stmtNSI
        returnN $ \l -> Labeled l i s

stmtNoTrail :: (Parsable l) => P (StmtNode l)
stmtNoTrail = tP (
    -- empty statement
    try ((toNode . Empty) <$ semiColon) <|>
    -- assertions
    endSemi ( do
        tok KW_Assert
        e   <- expParser
        me2 <- opt $ colon >> expParser
        returnN $ \l -> Assert l e me2) <|>
    -- switch stmts
    (do tok KW_Switch
        e  <- parens expParser
        sb <- switchBlock
        returnN $ \l -> Switch l e sb) <|>
    -- do-while loops
    endSemi (do
        tok KW_Do
        s <- stmt
        tok KW_While
        e <- parens expParser
        returnN $ \l -> Do l s e) <|>
    -- break
    endSemi (do
        tok KW_Break
        mi <- opt ident
        returnN $ \l -> Break l mi) <|>
    -- continue
    endSemi (do
        tok KW_Continue
        mi <- opt ident
        returnN $ \l -> Continue l mi) <|>
    -- return
    endSemi (do
        tok KW_Return
        me <- opt expParser
        returnN $ \l -> Return l me) <|>
    -- synchronized
    (do tok KW_Synchronized
        e <- parens expParser
        b <- blockParser
        returnN $ \l -> Synchronized l e b) <|>
    -- throw
    endSemi (do
        tok KW_Throw
        e <- expParser
        returnN $ \l -> Throw l e) <|>
    -- try-catch, both with and without a finally clause
    (do tok KW_Try
        res <- fromMaybe [] <$> optionMaybe (parens tryResources)
        b <- blockParser
        c <- list catch
        mf <- opt $ tok KW_Finally >> blockParser
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        returnN $ \l -> Try l res b c mf)) <|>
    -- inner blockParser
    (wrap blockParser) <|>
    -- expressions as stmts
    wrap (endSemi stmtExp)

-- For loops

forInitParser :: (Parsable l) => P (ForInitNode l)
forInitParser = tP $ try (do
                        (m,t,vds) <- localVarDecl
                        returnN $ \l -> ForLocalVars l m t vds) <|>
    (wrapL $ (flip ForInitExps) <$> seplist1 stmtExp comma)

forUp :: (Parsable l) => P [ExpNode l]
forUp = seplist1 stmtExp comma

-- Switches

switchBlock :: (Parsable l) => P [SwitchBlock l]
switchBlock = braces $ list switchStmt

switchStmt :: (Parsable l) => P (SwitchBlock l)
switchStmt = tP $ do
    lbl <- switchLabelParser
    bss <- list $ blockStmt
    return $ \l -> SwitchBlock l lbl bss

switchLabelParser :: (Parsable l) => P (SwitchLabelNode l)
switchLabelParser = (tP $ tok KW_Default >> colon >> return DefaultNode) <|>
    (do tok KW_Case
        e <- expParser
        colon
        return $ toNode e)

-- Try-catch clauses

tryResources :: (Parsable l) => P [TryResourceNode l]
tryResources = seplist tryRes semiColon
    where
      tryRes = tP $ try (do
            mods <- list modifier
            ty <- refType
            vars <- varDecls
            return $ \l -> TryResourceVar l mods ty vars
        ) <|> (wrapL $ TryResourceFinalVar <$$> ident)

catch :: (Parsable l) => P (Catch l)
catch = tP $ do
    tok KW_Catch
    fp <- parens formalParam
    b  <- blockParser
    return $ \l -> Catch l fp b

----------------------------------------------------------------------------
-- Expressions

stmtExp :: (Parsable l) => P (ExpNode l)
stmtExp = try preIncDec
    <|> try postIncDec
    <|> try (wrap assignment)
    -- There are sharing gains to be made by unifying these two
    <|> try (wrap methodInvocationExp)
    <|> try lambdaExp
    <|> try methodRef
    <|> instanceCreation

preIncDec :: (Parsable l) => P (ExpNode l)
preIncDec = do
    op <- preIncDecOp
    e <- unaryExp
    return $ op e

postIncDec :: (Parsable l) => P (ExpNode l)
postIncDec = do
    e <- postfixExpNES
    ops <- list1 postfixOp
    return $ foldl (\a s -> s a) e ops

assignment :: (Parsable l) => P (Assign l)
assignment = tP $ do
    lh <- lhs
    op <- assignOpParser
    e  <- assignExp
    return $ \l -> Assign l lh op e

lhs :: (Parsable l) => P (LhsNode l)
lhs = try (wrap fieldAccessParser)
    <|> try (wrap arrayAccess)
    <|> (wrapP $ NameLhs <$$> name)



expParser :: (Parsable l) => P (ExpNode l)
expParser = assignExp

assignExp :: (Parsable l) => P (ExpNode l)
assignExp = try methodRef <|> try lambdaExp <|> try(wrap assignment) <|> condExp

condExp :: (Parsable l) => P (ExpNode l)
condExp = do
    ie <- infixExp
    ces <- list $ wrapE condExpSuffix
    return $ foldl (\a s -> s a) ie ces

condExpSuffix :: (Parsable l) => P (Exp l (Cond l))
condExpSuffix =  do
    tok Op_Query
    th <- expParser
    colon
    el <- condExp
    return $ \ce l -> Cond l ce th el

infixExp :: (Parsable l) => P (ExpNode l)
infixExp = infixExpWithOperators infixOperators

-- See Note [Parsing operators]
infixExpWithOperators :: (Parsable l) => [P Op] -> P (ExpNode l)
infixExpWithOperators [] = unaryExp
infixExpWithOperators (op : ops) = do
    ue <- infixExpWithOperators ops
    ies <- list (infixExpSuffix op ops)
    return $ foldl (\a s -> s a) ue ies

infixExpSuffix :: (Parsable l) => P Op -> [P Op] -> P (Exp l (ExpNode l))
infixExpSuffix infixOp ops =
    (wrapE $ do
        op <- infixOp
        e2 <- infixExpWithOperators ops
        return $ \e1 l -> BinOp l e1 op e2) <|>

    -- FIXME 'instanceof' should have the same precedence as relational operators
    (wrapE $ tok KW_Instanceof >> (\t e1 l -> InstanceOf l e1 t) <$> refType)

unaryExp :: (Parsable l) => P (ExpNode l)
unaryExp = try preIncDec <|>
    try (do
        op <- prefixOp
        ue <- unaryExp
        return $ op ue) <|>
    try (tP $ do
        t <- parens ttype
        e <- unaryExp
        returnN $ \l -> Cast l t e) <|>
    postfixExp

postfixExpNES :: (Parsable l) => P (ExpNode l)
postfixExpNES = primaryParser

postfixExp :: (Parsable l) => P (ExpNode l)
postfixExp = do
    pe <- postfixExpNES
    ops <- list postfixOp
    return $ foldl (\a s -> s a) pe ops


primaryParser :: (Parsable l) => P (ExpNode l)
primaryParser = tP $ primaryNPS |>> primarySuffix

primaryNPS :: (Parsable l) => P (ExpNode l)
primaryNPS = try arrayCreation <|> primaryNoNewArrayNPS

-- primaryNoNewArray = startSuff primaryNoNewArrayNPS primarySuffix

primaryNoNewArrayNPS :: (Parsable l) => P (ExpNode l)
primaryNoNewArrayNPS =
    (wrapP $ Lit <$$> literalParser) <|>
    (wrapP $ This <$ tok KW_This) <|>
    parens expParser <|>
    -- TODO: These two following should probably be merged more
    try (tP $  do
        rt <- resultType
        period >> tok KW_Class
        returnN $ \l -> ClassLit l rt) <|>
    try (tP $ do
        t <- ttype
        period >> tok KW_This
        returnN $ \l -> QualifiedThis l t) <|>
    try (wrap instanceCreationNPS) <|>
    try (wrap methodInvocationNPS) <|>
    try (wrap fieldAccessNPS) <|>
    try (wrapP $ ExpName <$$> name) <|>
    wrap arrayAccessNPS

primarySuffix :: (Parsable l) => P (Exp l (ExpNode l))
primarySuffix = try (wrapE instanceCreationSuffix) <|>
    try (wrapE arrayAccessSuffix) <|>
    try ((wrapE . wrapE) methodInvocationSuffix) <|>
    ((wrapE . wrapE) fieldAccessSuffix)


instanceCreationNPS :: (Parsable l) => P (InstanceCreation l)
instanceCreationNPS = tP $
    do tok KW_New
       tas <- lopt typeArgsParser
       tds <- typeDeclSpecifier
       as  <- args
       mcb <- opt classBodyParser
       return $ \l -> InstanceCreation l tas tds as mcb

typeDeclSpecifier :: P TypeDeclSpecifier
typeDeclSpecifier = TypeDeclSpecifier <$> classType


instanceCreationSuffix :: (Parsable l) => P (Exp l (QualInstanceCreation l))
instanceCreationSuffix =
    do  period >> tok KW_New
        tas <- lopt typeArgsParser
        i   <- ident
        as  <- args
        mcb <- opt classBodyParser
        return $ \p l -> QualInstanceCreation l p tas i as mcb

instanceCreation :: (Parsable l) => P (ExpNode l)
instanceCreation = try (wrap instanceCreationNPS) <|> do
    p <- primaryNPS
    ss <- (list . tP) (flip <$> primarySuffix)
    let icp = foldl (\a s -> s a) p ss
    case icp of
     QualInstanceCreationNode {} -> wrap $ return icp
     _ -> fail ""


lambdaParamsParser :: (Parsable l) => P (LambdaParamsNode l)
lambdaParamsParser = tP (try (wrapL $ LambdaSingleParam <$$> ident)
               <|> try ( (parens . wrapL) $ LambdaFormalParams <$$> seplist formalParam comma)
               <|> (parens . wrapL) $ LambdaInferredParams <$$> seplist ident comma)
               where
                sglP,lfp,lifp :: P (l -> LambdaParamsNode l)
                sglP = wrapL $ LambdaSingleParam <$$> ident
                lfp = (parens . wrapL) $ LambdaFormalParams <$$> seplist formalParam comma
                lifp = (parens . wrapL) $ LambdaInferredParams <$$> seplist ident comma

lambdaExp :: (Parsable l) => P (ExpNode l)
lambdaExp = wrapP $ Lambda <$$> (lambdaParamsParser <* tok LambdaArrow)
            <**> (((try . wrap) blockParser)
                 <|> (wrap expParser))

methodRef :: (Parsable l) => P (ExpNode l)
methodRef = tP $ do
                n <- name
                tok MethodRefSep
                i <- ident
                returnN $ \l -> MethodRef l n i

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

fieldAccessNPS :: (Parsable l) => P (FieldAccessNode l)
fieldAccessNPS = tP $
    (do tok KW_Super >> period
        i <- ident
        returnN $ \l -> SuperFieldAccess l i) <|>
    (do n <- name
        period >> tok KW_Super >> period
        i <- ident
        returnN $ \l -> ClassFieldAccess l n i)

fieldAccessSuffix :: (Parsable l) => P (Exp l (PrimaryFieldAccess l))
fieldAccessSuffix = do
    period
    i <- ident
    return $ \p l -> PrimaryFieldAccess l p i

fieldAccessParser :: (Parsable l) => P (FieldAccessNode l)
fieldAccessParser = try fieldAccessNPS <|> do
    p <- primaryNPS
    ss <- (list . tP) (flip <$> primarySuffix)
    let fap = foldl (\a s -> s a) p ss
    case fap of
     FieldAccessNode fa -> return fa
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

methodInvocationNPS :: (Parsable l) => P (MethodInvocationNode l)
methodInvocationNPS = tP $
    (do tok KW_Super >> period
        rts <- (map snd) <$> lopt refTypeArgs
        i   <- ident
        as  <- args
        returnN $ \l -> SuperMethodCall l rts i as) <|>
    (do n <- name
        f <- (do as <- args
                 return $ \na l -> toNode $ MethodCall l na as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- (map snd) <$> lopt refTypeArgs
                i   <- ident
                as  <- args
                let typeM = \na l -> toNode $ TypeMethodCall l na rts i as
                let classM = const $ \na l -> toNode $ ClassMethodCall l na rts i as
                return $ maybe typeM classM msp)
        return $ f n)

methodInvocationSuffix :: (Parsable l) => P (Exp l (PrimaryMethodCall l))
methodInvocationSuffix = do
        period
        _ <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ \p l -> PrimaryMethodCall l p [] i as

methodInvocationExp :: (Parsable l) => P (ExpNode l)
methodInvocationExp = try (do
    p <- primaryNPS
    ss <- (list . tP) (flip <$> primarySuffix)
    let mip = foldl (\a s -> s a) p ss
    case mip of
     MethodInvNode _ -> return mip
     _ -> fail "") <|>
     (wrap methodInvocationNPS)

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
arrayAccessNPS = tP $ do
    n <- wrap $ tP $ (flip ExpName) <$> name
    e <- list1 $ brackets expParser
    return $ \l -> ArrayIndex l n e

arrayAccessSuffix :: (Parsable l) => P (Exp l (ArrayIndex l))
arrayAccessSuffix = do
    e <- list1 $ brackets expParser
    return $ \ref l -> ArrayIndex l ref e

arrayAccess :: (Parsable l) => P (ArrayIndex l)
arrayAccess = try arrayAccessNPS <|> do
    p <- primaryNoNewArrayNPS
    ss <- (list . tP) (flip <$> primarySuffix)
    let aap = foldl (\a s -> s a) p ss
    case aap of
     ArrayAccessNode ain -> return ain
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

arrayCreation :: (Parsable l) => P (ExpNode l)
arrayCreation = tP $ do
    tok KW_New
    t <- nonArrayType
    f <- try (do
             ds <- list1 $ brackets empty
             ai <- arrayInit
             return $ \tt l -> toNode $ ArrayCreateInit l tt (length ds) ai) <|>
         (do des <- list1 $ try $ brackets expParser
             ds  <- list  $ brackets empty
             return $ \tt l -> toNode $ ArrayCreate l tt des (length ds))
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

preIncDecOp, prefixOp, postfixOp :: (Parsable l) => P (ExpNode l -> ExpNode l)
preIncDecOp =
    (tok Op_PPlus >> return PreIncrementNode) <|>
    (tok Op_MMinus >> return PreDecrementNode)
prefixOp =
    (tok Op_Bang  >> return PreNotNode ) <|>
    (tok Op_Tilde >> return PreBitComplNode ) <|>
    (tok Op_Plus  >> return PrePlusNode ) <|>
    (tok Op_Minus >> return PreMinusNode )
postfixOp =
    (tok Op_PPlus  >> return PostIncrementNode ) <|>
    (tok Op_MMinus >> return PostDecrementNode )

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

refTypeList :: (Parsable l) => P [(l, RefType)]
refTypeList = seplist1 (tP $ (\r l -> (l, r)) <$> refType) comma

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

refTypeArgs :: (Parsable l) => P [(l, RefType)]
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


startSuff, (|>>) :: Parsable l => P a -> P (a -> l -> a) -> P a
startSuff start suffix = do
    x <- start
    ss <- (list . tP) (flip <$> suffix)
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
type Exp l a = ExpNode l -> l -> a

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

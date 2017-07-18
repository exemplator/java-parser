{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Java.Parser (
    Parsable(..),

    parseCompilationUnit, parser,

    compilationUnit, packageDecl, importDecl, typeDecl,

    classDecl, interfaceDecl,

    memberDecl, fieldDecl, methodDecl, constrDecl,
    interfaceMemberDecl, absMethodDecl,

    formalParams, formalParam,

    modifier,

    varDecls, varDecl,

    block, blockStmt, stmt,

    stmtExp, exp, primary, literal,

    ttype, primType, refType, classType, resultType, typeDeclSpecifier,

    lambdaExp, methodRef,

    typeParams, typeParam,

    name, ident,

    empty, list, list1, seplist, seplist1, opt, bopt, lopt,

    comma, semiColon, period, colon

    ) where

import           Language.Java.Lexer    (L (..), Token (..), lexer)
import           Language.Java.Position
import           Language.Java.Pretty   (pretty)
import           Language.Java.Syntax

import           Text.Parsec            hiding (Empty)
import           Text.Parsec.Pos

import           Data.Char              (toLower)
import           Data.Maybe             (catMaybes, isJust, listToMaybe, maybe)
import           Prelude                hiding (exp, (>>), (>>=))
import qualified Prelude                as P ((>>), (>>=))

import           Control.Applicative    ((<$), (<$>), (<*), (<*>))

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
    toParser :: (l -> a) -> P a

instance Parsable Segment where
    toParser constr = do
        start <- getPosition
        result <- pure constr
        end <- getPosition
        let segmt = sourcePosToSegment start end
        return (result segmt)

tP :: (Parsable l) => (l -> a) -> P a
tP = toParser

(<$$>) :: (Parsable l) => (l -> a -> b) -> P a -> P b
(<$$>) constr pa = tP constr <*> pa
infixl 4 <$$>

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

compilationUnit :: Parsable l => P (CompilationUnit l)
compilationUnit = do
        comp <- tP CompilationUnit
        mpd <- opt packageDecl
        ids <- list importDecl
        tds <- list typeDecl
        eof
        return $ comp mpd ids (catMaybes tds)

packageDecl :: (Parsable l) => P (PackageDecl l)
packageDecl = do
    pgkDec <- tP PackageDecl
    tok KW_Package
    pkg <- fullQualiPkg
    semiColon
    return $ pgkDec pkg

importDecl :: (Parsable l) => P (ImportDecl l)
importDecl = do
    imp <- tP ImportDecl
    tok KW_Import
    st <- bopt $ tok KW_Static
    n  <- name
    ds <- bopt $ period >> tok Op_Star
    semiColon
    return $ imp st n ds

typeDecl :: (Parsable l) => P (Maybe (TypeDecl l))
typeDecl = Just <$> classOrInterfaceDecl <|>
            const Nothing <$> semiColon

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

classOrInterfaceDecl :: (Parsable l) => P (TypeDecl l)
classOrInterfaceDecl = do
    ms <- list modifier
    de <- (do cl <- tP ClassTypeDecl
              cd <- classDecl
              return $ \mst -> cl (cd mst)) <|>
          (do ifT <- tP InterfaceTypeDecl
              idecl <- annInterfaceDecl <|> interfaceDecl
              return $ \mst -> ifT (idecl mst))
    return $ de ms

classDecl :: (Parsable l) => P (Mod l (ClassDecl l))
classDecl = normalClassDecl <|> enumClassDecl

normalClassDecl :: (Parsable l) => P (Mod l (ClassDecl l))
normalClassDecl = do
    clDec <- tP ClassDecl
    tok KW_Class
    i   <- ident
    tps <- lopt typeParams
    mex <- opt extends
    imp <- lopt implements
    bod <- classBody
    return $ \ms -> clDec ms i tps (fmap head mex) imp bod

extends :: P [RefType]
extends = tok KW_Extends >> refTypeList

implements :: P [RefType]
implements = tok KW_Implements >> refTypeList

enumClassDecl :: (Parsable l) => P (Mod l (ClassDecl l))
enumClassDecl = do
    enDec <- tP EnumDecl
    tok KW_Enum
    i   <- ident
    imp <- lopt implements
    bod <- enumBody
    return $ \ms -> enDec ms i imp bod

classBody :: (Parsable l) => P (ClassBody l)
classBody = toParser ClassBody <*> braces classBodyStatements

enumBody :: (Parsable l) => P (EnumBody l)
enumBody = braces $ do
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
    mcb <- opt classBody
    return $ enC idt as mcb

enumBodyDecls :: (Parsable l) => P [Decl l]
enumBodyDecls = semiColon >> classBodyStatements

classBodyStatements :: (Parsable l) => P [Decl l]
classBodyStatements = catMaybes <$> list classBodyStatement

-- Interface declarations

annInterfaceDecl :: (Parsable l) => P (Mod l (InterfaceDecl l))
annInterfaceDecl = do
    ifD <- tP InterfaceDecl
    tok KW_AnnInterface
    idt  <- ident
    tps <- lopt typeParams
    exs <- lopt extends
    bod <- interfaceBody
    return $ \ms -> ifD InterfaceAnnotation ms idt tps exs bod

interfaceDecl :: (Parsable l) => P (Mod l (InterfaceDecl l))
interfaceDecl = do
    ifD <- tP InterfaceDecl
    tok KW_Interface
    idt  <- ident
    tps <- lopt typeParams
    exs <- lopt extends
    bod <- interfaceBody
    return $ \ms -> ifD InterfaceNormal ms idt tps exs bod

interfaceBody :: (Parsable l) => P (InterfaceBody l)
interfaceBody = (. catMaybes) <$> tP InterfaceBody <*>
    braces (list interfaceBodyDecl)

-- Declarations

classBodyStatement :: (Parsable l) => P (Maybe (Decl l))
classBodyStatement =
    try (do
       _ <- list1 semiColon
       return Nothing) <|>
    try ( do
       initDecl <- tP InitDecl
       mst <- bopt (tok KW_Static)
       blk <- block
       return $ Just $ initDecl mst blk) <|>
    (do mDec <- tP MemberDecl
        ms  <- list modifier
        dec <- memberDecl
        return $ Just $ mDec (dec ms))

memberDecl :: (Parsable l) => P (Mod l (MemberDecl l))
memberDecl =
    try (do
        mDec <- tP MemberClassDecl
        cd <- classDecl
        return $ \ ms -> mDec (cd ms))
    <|>
    try
        (do mInDec <- tP MemberInterfaceDecl
            idecl <- try annInterfaceDecl <|> try interfaceDecl
            return $ mInDec . idecl) <|>

    try fieldDecl <|>
    try methodDecl <|>
    constrDecl

fieldDecl :: (Parsable l) => P (Mod l (MemberDecl l))
fieldDecl = endSemi $ do
    fDec <- tP FieldDecl
    typ <- ttype
    vds <- varDecls
    return $ \ms -> fDec ms typ vds

methodDecl :: (Parsable l) => P (Mod l (MemberDecl l))
methodDecl = do
    mDec <- tP MethodDecl
    tps <- lopt typeParams
    rt  <- resultType
    idt  <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- methodBody
    return $ \ms -> mDec ms tps rt idt fps thr Nothing bod

methodBody :: (Parsable l) => P (MethodBody l)
methodBody = MethodBody <$$>
    (const Nothing <$> semiColon <|> Just <$> block)


constrDecl :: (Parsable l) => P (Mod l (MemberDecl l))
constrDecl = do
    constDec <- tP ConstructorDecl
    tps <- lopt typeParams
    idt  <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- constrBody
    return $ \ms -> constDec ms tps idt fps thr bod

constrBody :: (Parsable l) => P (ConstructorBody l)
constrBody = braces $ do
    cstBod <- tP ConstructorBody
    mec <- opt (try explConstrInv)
    bss <- list blockStmt
    return $ cstBod mec bss

explConstrInv :: (Parsable l) => P (ExplConstrInv l)
explConstrInv = endSemi $
    try ( do
        thisInv <- tP ThisInvoke
        tas <- lopt refTypeArgs
        tok KW_This
        as  <- args
        return $ thisInv tas as) <|>
    try ( do
        sInv <- tP SuperInvoke
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ sInv tas as) <|>
    (do priInv <- tP PrimarySuperInvoke
        pri <- primary
        period
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ priInv pri tas as)

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDecl :: (Parsable l) => P (Maybe (MemberDecl l))
interfaceBodyDecl = semiColon >> return Nothing <|>
    do ms  <- list modifier
       imd <- interfaceMemberDecl
       return $ Just (imd ms)

interfaceMemberDecl :: (Parsable l) => P (Mod l (MemberDecl l))
interfaceMemberDecl =
    (do mClDec <- tP MemberClassDecl
        cd  <- classDecl
        return $ \ms -> mClDec (cd ms)) <|>
    (do miDec <- tP MemberInterfaceDecl
        idt  <- try annInterfaceDecl <|> try interfaceDecl
        return $ \ms -> miDec (idt ms)) <|>
    try fieldDecl <|>
    absMethodDecl

absMethodDecl :: (Parsable l) => P (Mod l (MemberDecl l))
absMethodDecl = do
    meBod <- tP MethodBody
    meDec <- tP MethodDecl
    tps <- lopt typeParams
    rt  <- resultType
    idt  <- ident
    fps <- formalParams
    thr <- lopt throws
    def <- opt defaultValue
    semiColon
    return $ \ms -> meDec ms tps rt idt fps thr def (meBod Nothing)

defaultValue :: (Parsable l) => P (Exp l)
defaultValue = tok KW_Default >> exp

throws :: (Parsable l) => P [ExceptionType l]
throws = (tok KW_Throws >> refTypeList) >>= mapM (\x -> tP ExceptionType <*> pure x)

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
    <|> Annotation <$$> annotation

annotation :: (Parsable l) => P (Annotation l)
annotation = flip ($) <$ tok Op_AtSign <*> name <*> (
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
    <|> EVAnn <$$> annotation


----------------------------------------------------------------------------
-- Variable declarations

varDecls :: (Parsable l) => P [VarDecl l]
varDecls = seplist1 varDecl comma

varDecl :: (Parsable l) => P (VarDecl l)
varDecl = do
    vid <- varDeclId
    mvi <- opt $ tok Op_Equal >> varInit
    return $ VarDecl vid mvi

varDeclId :: (Parsable l) => P (VarDeclId l)
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

varInit :: (Parsable l) => P (VarInit l)
varInit =
    InitArray <$$> arrayInit <|>
    InitExp   <$$> exp

arrayInit :: (Parsable l) => P (ArrayInit l)
arrayInit = braces $ do
    arrayInt <- tP ArrayInit
    vis <- seplist varInit comma
    _ <- opt comma
    return $ arrayInt vis

----------------------------------------------------------------------------
-- Statements

block :: (Parsable l) => P (Block l)
block = braces $ Block <$$> list blockStmt

blockStmt :: (Parsable l) => P (BlockStmt l)
blockStmt =
    try ( do
        localClass <- tP LocalClass
        ms  <- list modifier
        cd  <- classDecl
        return $ localClass (cd ms)) <|>
    try ( do
        localVar <- tP LocalVars
        (m,t,vds) <- endSemi localVarDecl
        return $ localVar m t vds) <|>
    BlockStmt <$$> stmt

stmt :: (Parsable l) => P (Stmt l)
stmt = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e   <- parens exp
        try (do
               ifThenElse <- tP IfThenElse
               th <- stmtNSI
               tok KW_Else
               el <- stmt
               return $ ifThenElse e th el) <|>
           (do ifThen <- tP IfThen
               th <- stmt
               return $ ifThen e th)
    whileStmt = do
        while <- tP While
        tok KW_While
        e   <- parens exp
        s   <- stmt
        return $ while e s
    forStmt = do
        tok KW_For
        f <- parens $
            try ( do
                basicFor <- tP BasicFor
                fi <- opt forInit
                semiColon
                e  <- opt exp
                semiColon
                fu <- opt forUp
                return $ basicFor fi e fu) <|>
            (do enhancedFor <- tP EnhancedFor
                ms <- list modifier
                t  <- ttype
                i  <- ident
                colon
                e  <- exp
                return $ enhancedFor ms t i e)
        s <- stmt
        return $ f s
    labeledStmt = try $ do
        labeled <- tP Labeled
        lbl <- ident
        colon
        s   <- stmt
        return $ labeled lbl s

stmtNSI :: (Parsable l) => P (Stmt l)
stmtNSI = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
        ifThenElse <- tP IfThenElse
        tok KW_If
        e  <- parens exp
        th <- stmtNSI
        tok KW_Else
        el <- stmtNSI
        return $ ifThenElse e th el
    whileStmt = do
        while <- tP While
        tok KW_While
        e <- parens exp
        s <- stmtNSI
        return $ while e s
    forStmt = do
        tok KW_For
        f <- parens $ try ( do
            basic <- tP BasicFor
            fi <- opt forInit
            semiColon
            e  <- opt exp
            semiColon
            fu <- opt forUp
            return $ basic fi e fu)
            <|> (do
            enhancedFor <- tP EnhancedFor
            ms <- list modifier
            t  <- ttype
            i  <- ident
            colon
            e  <- exp
            return $ enhancedFor ms t i e)
        s <- stmtNSI
        return $ f s
    labeledStmt = try $ do
        labeled <- tP Labeled
        i <- ident
        colon
        s <- stmtNSI
        return $ labeled i s

stmtNoTrail :: (Parsable l) => P (Stmt l)
stmtNoTrail =
    -- empty statement
    const <$> tP Empty <*> semiColon <|>
    -- inner block
    StmtBlock <$$> block <|>
    -- assertions
    endSemi ( do
        assert <- tP Assert
        tok KW_Assert
        e   <- exp
        me2 <- opt $ colon >> exp
        return $ assert e me2) <|>
    -- switch stmts
    (do switch <- tP Switch
        tok KW_Switch
        e  <- parens exp
        sb <- switchBlock
        return $ switch e sb) <|>
    -- do-while loops
    endSemi (do
        doNode <- tP Do
        tok KW_Do
        s <- stmt
        tok KW_While
        e <- parens exp
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
        me <- opt exp
        return $ ret me) <|>
    -- synchronized
    (do sync <- tP Synchronized
        tok KW_Synchronized
        e <- parens exp
        b <- block
        return $ sync e b) <|>
    -- throw
    endSemi (do
        thr <- tP Throw
        tok KW_Throw
        e <- exp
        return $ thr e) <|>
    -- try-catch, both with and without a finally clause
    (do tryNode <- tP Try
        tok KW_Try
        b <- block
        c <- list catch
        mf <- opt $ tok KW_Finally >> block
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        return $ tryNode b c mf) <|>
    -- expressions as stmts
    ExpStmt <$$> endSemi stmtExp

-- For loops

forInit :: (Parsable l) => P (ForInit l)
forInit = try (do forLocalVars <- tP ForLocalVars
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
    lbl <- switchLabel
    bss <- list blockStmt
    return $ switch lbl bss

switchLabel :: (Parsable l) => P (SwitchLabel l)
switchLabel = (tok KW_Default >> colon >> tP Default) <|>
    (do switch <- tP SwitchCase
        tok KW_Case
        e <- exp
        colon
        return $ switch e)

-- Try-catch clauses

catch :: (Parsable l) => P (Catch l)
catch = do
    catch <- tP Catch
    tok KW_Catch
    fp <- parens formalParam
    b  <- block
    return $ catch fp b

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
    op <- assignOp
    e  <- assignExp
    return $ assign lh op e

lhs :: (Parsable l) => P (Lhs l)
lhs = try (FieldLhs <$$> fieldAccess)
    <|> try (ArrayLhs <$$> arrayAccess)
    <|> NameLhs <$$> name



exp :: (Parsable l) => P (Exp l)
exp = assignExp

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
    th <- exp
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
    (do binOp <- tP BinOp
        op <- infixOp
        e2 <- infixExpWithOperators ops
        return $ \e1 -> binOp e1 op e2) <|>

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
postfixExpNES = primary

postfixExp :: (Parsable l) => P (Exp l)
postfixExp = do
    pe <- postfixExpNES
    ops <- list postfixOp
    return $ foldl (\a s -> s a) pe ops


primary :: (Parsable l) => P (Exp l)
primary = primaryNPS |>> primarySuffix

primaryNPS :: (Parsable l) => P (Exp l)
primaryNPS = try arrayCreation <|> primaryNoNewArrayNPS

-- primaryNoNewArray = startSuff primaryNoNewArrayNPS primarySuffix

primaryNoNewArrayNPS :: (Parsable l) => P (Exp l)
primaryNoNewArrayNPS =
    Lit <$$> literal <|>
    const <$> tP This <*> tok KW_This <|>
    parens exp <|>
    -- TODO: These two following should probably be merged more
    try ( do
        classLit <- tP ClassLit
        rt <- resultType
        period >> tok KW_Class
        return $ classLit rt) <|>
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
       tas <- lopt typeArgs
       tds <- typeDeclSpecifier
       as  <- args
       mcb <- opt classBody
       return $ instCr tas tds as mcb

typeDeclSpecifier :: P TypeDeclSpecifier
typeDeclSpecifier =
    do ct <- classType
       return $ TypeDeclSpecifier ct


instanceCreationSuffix :: (Parsable l) => P (Exp l -> Exp l)
instanceCreationSuffix =
     do qualCre <- tP QualInstanceCreation
        period >> tok KW_New
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        return $ \p -> qualCre p tas i as mcb

instanceCreation :: (Parsable l) => P (Exp l)
instanceCreation = try instanceCreationNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let icp = foldl (\a s -> s a) p ss
    case icp of
     QualInstanceCreation {} -> return icp
     _ -> fail ""


lambdaParams :: (Parsable l) => P (LambdaParams l)
lambdaParams = try (LambdaSingleParam <$$> ident)
               <|> try (parens $ LambdaFormalParams <$$> seplist formalParam comma)
               <|> parens (LambdaInferredParams <$$> seplist ident comma)

lambdaExp :: (Parsable l) => P (Exp l)
lambdaExp = Lambda <$$> (lambdaParams <* tok LambdaArrow)
            <*> ((LambdaBlock <$$> try block)
                 <|> (LambdaExpression <$$> exp))

methodRef :: (Parsable l) => P (Exp l)
methodRef = MethodRef <$$> (name <*  tok MethodRefSep)
            <*> ident

{-
instanceCreation =
    (do tok KW_New
        tas <- lopt typeArgs
        ct  <- classType
        as  <- args
        mcb <- opt classBody
        return $ InstanceCreation tas ct as mcb) <|>
    (do p   <- primary
        period >> tok KW_New
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
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

fieldAccess :: (Parsable l) => P (FieldAccess l)
fieldAccess = try fieldAccessNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let fap = foldl (\a s -> s a) p ss
    case fap of
     FieldAccess _ fa -> return fa
     _ -> fail ""

{-
fieldAccess :: P FieldAccess
fieldAccess = try fieldAccessNPS <|> do
    p <- primary
    fs <- fieldAccessSuffix
    return (fs p)
-}

{-
fieldAccess :: P FieldAccess
fieldAccess =
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccess i) <|>
    (try $ do
        n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess n i) <|>
    (do p <- primary
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
    (do p <- primary
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
args = parens $ seplist exp comma

-- Arrays

arrayAccessNPS :: (Parsable l) => P (ArrayIndex l)
arrayAccessNPS = do
    arrInd <- tP ArrayIndex
    exName <- tP ExpName
    n <- name
    e <- list1 $ brackets exp
    return $ arrInd (exName n) e

arrayAccessSuffix :: (Parsable l) => P (Exp l -> ArrayIndex l)
arrayAccessSuffix = do
    arrInd <- tP ArrayIndex
    e <- list1 $ brackets exp
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
    e   <- brackets exp
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
             des <- list1 $ try $ brackets exp
             ds  <- list  $ brackets empty
             return $ \tt -> arrCr tt des (length ds))
    return $ f t

literal :: P Literal
literal =
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

assignOp :: P AssignOp
assignOp =
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
        constructClassType (a, b) = if null a then WithoutPackage b else WithPackage (FullQualiPackage a) b

        parseClass = sepListEndOptBy (classTypeSpec typeArgs) (classTypeSpec typeArgsWithDiamond) period

        typeArgsWithDiamond = try typeArgs <|> (:[]) <$> typeDiamond

classTypeSpec :: P [TypeArgument] -> P (Ident, [TypeArgument])
classTypeSpec args = do
    i   <- ident
    tas <- lopt args
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

typeArgs :: P [TypeArgument]
typeArgs = angles $ seplist1 typeArg comma

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

type Mod l a = [Modifier l] -> a

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

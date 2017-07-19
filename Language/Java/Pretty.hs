module Language.Java.Pretty where

import           Data.Char            (toLower)
import           Text.PrettyPrint
import           Text.Printf          (printf)

import           Language.Java.Syntax


prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec inheritedPrec currentPrec t
    | inheritedPrec <= 0          = t
    | inheritedPrec < currentPrec = parens t
    | otherwise                   = t

class Pretty a where
  pretty :: a -> Doc
  pretty = prettyPrec 0

  prettyPrec :: Int -> a -> Doc
  prettyPrec _ = pretty

-----------------------------------------------------------------------
-- Packages

instance (Show l) => Pretty (CompilationUnit l) where
  prettyPrec p (CompilationUnit _ mpd ids tds) =
    vcat $ (maybePP p mpd: map (prettyPrec p) ids) ++ map (prettyPrec p) tds

instance Pretty (PackageDecl l) where
  prettyPrec p (PackageDecl _ name) = text "package" <+> prettyPrec p name <> semi

instance Pretty (ImportDecl l) where
  prettyPrec p (ImportDecl _ st pkg) =
    text "import" <+> opt st (text "static")
                  <+> prettyPrec p pkg
                  <> semi

-----------------------------------------------------------------------
-- Declarations

instance (Show l) => Pretty (TypeDecl l) where
  prettyPrec p (ClassTypeDecl _ cd) = prettyPrec p cd
  prettyPrec p (InterfaceTypeDecl _ idecl) = prettyPrec p idecl

instance (Show l) => Pretty (ClassDecl l) where
  prettyPrec p (EnumDecl _ mods ident impls body) =
    hsep [hsep (map (prettyPrec p) mods)
          , text "enum"
          , prettyPrec p ident
          , ppImplements p impls
         ] $$ prettyPrec p body

  prettyPrec p (ClassDecl _ mods ident tParams mSuper impls body) =
    hsep [hsep (map (prettyPrec p) mods)
          , text "class"
          , prettyPrec p ident
          , ppTypeParams p tParams
          , ppExtends p (maybe [] return mSuper)
          , ppImplements p impls
         ] $$ prettyPrec p body

instance (Show l) => Pretty (ClassBody l) where
  prettyPrec p (ClassBody _ ds) =
    braceBlock (map (prettyPrec p) ds)

instance (Show l) => Pretty (EnumBody l) where
  prettyPrec p (EnumBody _ cs ds) =
    braceBlock $
        punctuate comma (map (prettyPrec p) cs) ++
        opt (not $ null ds) semi : map (prettyPrec p) ds

instance (Show l) => Pretty (EnumConstant l) where
  prettyPrec p (EnumConstant _ ident args mBody) =
    prettyPrec p ident
        -- needs special treatment since even the parens are optional
        <> opt (not $ null args) (ppArgs args)
      $$ maybePP p mBody

instance (Show l) => Pretty (InterfaceDecl l) where
  prettyPrec p (InterfaceDecl _ kind mods ident tParams impls body) =
    hsep [hsep (map (prettyPrec p) mods)
          , text (if kind == InterfaceNormal then "interface" else "@interface")
          , prettyPrec p ident
          , ppTypeParams p tParams
          , ppExtends p impls
         ] $$ prettyPrec p body

instance (Show l) => Pretty (InterfaceBody l) where
  prettyPrec p (InterfaceBody _ mds) =
    braceBlock (map (prettyPrec p) mds)

instance (Show l) => Pretty (Decl l) where
  prettyPrec p (MemberDecl _ md) = prettyPrec p md
  prettyPrec p (InitDecl _ b bl) =
    opt b (text "static") <+> prettyPrec p bl

instance (Show l) => Pretty (MemberDecl l) where
  prettyPrec p (FieldDecl _ mods t vds) =
    hsep (map (prettyPrec p) mods ++ prettyPrec p t:punctuate (text ",") (map (prettyPrec p) vds)) <> semi

  prettyPrec p (MethodDecl _ mods tParams mt ident fParams throws def body) =
    hsep [hsep (map (prettyPrec p) mods)
          , ppTypeParams p tParams
          , ppResultType p mt
          , prettyPrec p ident
          , ppArgs fParams
          , ppThrows p throws
          , ppDefault p def
         ] $$ prettyPrec p body

  prettyPrec p (ConstructorDecl _ mods tParams ident fParams throws body) =
    hsep [hsep (map (prettyPrec p) mods)
          , ppTypeParams p tParams
          , prettyPrec p ident
          , ppArgs fParams
          , ppThrows p throws
         ] $$ prettyPrec p body

  prettyPrec p (MemberClassDecl _ cd) = prettyPrec p cd
  prettyPrec p (MemberInterfaceDecl _ idecl) = prettyPrec p idecl

instance (Show l) => Pretty (VarDecl l) where
  prettyPrec p (VarDecl vdId Nothing) = prettyPrec p vdId
  prettyPrec p (VarDecl vdId (Just ie)) =
    (prettyPrec p vdId <+> char '=') <+> prettyPrec p ie

instance Pretty (VarDeclId l) where
  prettyPrec p (VarId _ ident) = prettyPrec p ident
  prettyPrec p (VarDeclArray _ vId) = prettyPrec p vId <> text "[]"

instance (Show l) => Pretty (VarInit l) where
  prettyPrec p (InitExp _ e) = prettyPrec p e
  prettyPrec p (InitArray _ (ArrayInit _ ai)) =
    text "{" <+> hsep (punctuate comma (map (prettyPrec p) ai)) <+> text "}"

instance (Show l) => Pretty (FormalParam l) where
  prettyPrec p (FormalParam _ mods t b vId) =
    hsep [hsep (map (prettyPrec p) mods)
          , prettyPrec p t <> opt b (text "...")
          , prettyPrec p vId
         ]

instance (Show l) => Pretty (MethodBody l) where
  prettyPrec p (MethodBody _ mBlock) = maybe semi (prettyPrec p) mBlock

instance (Show l) => Pretty (ConstructorBody l) where
  prettyPrec p (ConstructorBody _ mECI stmts) =
    braceBlock $ maybePP p mECI : map (prettyPrec p) stmts

instance (Show l) => Pretty (ExplConstrInv l) where
  prettyPrec p (ThisInvoke _ rts args) =
    ppTypeParams p rts <+> text "this" <> ppArgs args <> semi
  prettyPrec p (SuperInvoke _ rts args) =
    ppTypeParams p rts <+> text "super" <> ppArgs args <> semi
  prettyPrec p (PrimarySuperInvoke _ e rts args) =
    prettyPrec p e <> char '.' <>
      ppTypeParams p rts <+> text "super" <> ppArgs args <> semi

instance (Show l) => Pretty (Modifier l) where
  prettyPrec p (Annotation _ ann) = prettyPrec p ann $+$ nest (-1) ( text "")
  prettyPrec _ modifier = text . map toLower $ show modifier

instance (Show l) => Pretty (Annotation l) where
  prettyPrec p x = text "@" <> prettyPrec p (annName x) <> case x of
         MarkerAnnotation {} -> text ""
         SingleElementAnnotation {} -> text "(" <> prettyPrec p (annValue x) <> text ")"
         NormalAnnotation {} -> text "(" <> ppEVList p (annKV x) <> text ")"

ppEVList p = hsep . punctuate comma . map (\(k,v) -> prettyPrec p k <+> text "=" <+> prettyPrec p v)

instance (Show l) => Pretty (ElementValue l) where
  prettyPrec p (EVVal _ vi) = prettyPrec p vi
  prettyPrec p (EVAnn _ ann) = prettyPrec p ann

-----------------------------------------------------------------------
-- Statements


instance (Show l) => Pretty (Block l) where
  prettyPrec p (Block _ stmts) = braceBlock $ map (prettyPrec p) stmts

instance (Show l) => Pretty (BlockStmt l) where
  prettyPrec p (BlockStmt _ stmt) = prettyPrec p stmt
  prettyPrec p (LocalClass _ cd) = prettyPrec p cd
  prettyPrec p (LocalVars _ mods t vds) =
    hsep (map (prettyPrec p) mods) <+> prettyPrec p t <+>
      hsep (punctuate comma $ map (prettyPrec p) vds) <> semi

instance (Show l) => Pretty (Stmt l) where
  prettyPrec p (StmtBlock _ block) = prettyPrec p block
  prettyPrec _ (IfThen _ c th) =
    text "if" <+> parens (prettyPrec 0 c) $+$ prettyNestedStmt 0 th

  prettyPrec p (IfThenElse _ c th el) =
    text "if" <+> parens (prettyPrec p c) $+$ prettyNestedStmt 0 th $+$ text "else" $+$ prettyNestedStmt 0 el

  prettyPrec p (While _ c stmt) =
    text "while" <+> parens (prettyPrec p c) $+$ prettyNestedStmt 0 stmt

  prettyPrec p (BasicFor _ mInit mE mUp stmt) =
    text "for" <+> parens (hsep [maybePP p mInit, semi
                           , maybePP p mE, semi
                           , maybe empty (hsep . punctuate comma . map (prettyPrec p)) mUp
                          ]) $+$ prettyNestedStmt p stmt

  prettyPrec p (EnhancedFor _ mods t ident e stmt) =
    hsep [text "for"
          , parens $ hsep [
                  hsep (map (prettyPrec p) mods)
                , prettyPrec p t
                , prettyPrec p ident
                , colon
                , prettyPrec p e
               ]
          , prettyPrec p stmt
         ]

  prettyPrec _ (Empty _) = semi

  prettyPrec p (ExpStmt _ e) = prettyPrec p e <> semi

  prettyPrec p (Assert _ ass mE) =
    text "assert" <+> prettyPrec p ass
      <+> maybe empty ((colon <>) . prettyPrec p) mE <> semi

  prettyPrec p (Switch _ e sBlocks) =
    text "switch" <+> parens (prettyPrec p e)
      $$ braceBlock (map (prettyPrec p) sBlocks)

  prettyPrec p (Do _ stmt e) =
    text "do" $+$ prettyPrec p stmt <+> text "while" <+> parens (prettyPrec p e) <> semi

  prettyPrec p (Break _ mIdent) =
    text "break" <+> maybePP p mIdent <> semi

  prettyPrec p (Continue _ mIdent) =
    text "continue" <+> maybePP p mIdent <> semi

  prettyPrec p (Return _ mE) =
    text "return" <+> maybePP p mE <> semi

  prettyPrec p (Synchronized _ e block) =
    text "synchronized" <+> parens (prettyPrec p e) $$ prettyPrec p block

  prettyPrec p (Throw _ e) =
    text "throw" <+> prettyPrec p e <> semi

  prettyPrec p (Try _ block catches mFinally) =
    text "try" $$ prettyPrec p block $$
      vcat (map (prettyPrec p) catches ++ [ppFinally mFinally])
   where ppFinally Nothing = empty
         ppFinally (Just bl) = text "finally" <+> prettyPrec p bl

  prettyPrec p (Labeled _ ident stmt) =
    prettyPrec p ident <> colon <+> prettyPrec p stmt

instance (Show l) => Pretty (Catch l) where
  prettyPrec p (Catch _ fParam block) =
    hsep [text "catch", parens (prettyPrec p fParam)] $$ prettyPrec p block

instance (Show l) => Pretty (SwitchBlock l) where
  prettyPrec p (SwitchBlock _ lbl stmts) =
    vcat (prettyPrec p lbl : map (nest 2 . prettyPrec p) stmts)

instance (Show l) => Pretty (SwitchLabel l) where
  prettyPrec p (SwitchCase _ e) =
    text "case" <+> prettyPrec p e <> colon
  prettyPrec _ (Default _) = text "default:"

instance (Show l) => Pretty (ForInit l) where
  prettyPrec p (ForLocalVars _ mods t vds) =
    hsep $ map (prettyPrec p) mods ++
            prettyPrec p t: punctuate comma (map (prettyPrec p) vds)
  prettyPrec p (ForInitExps _ es) =
    hsep $ punctuate comma (map (prettyPrec p) es)


-----------------------------------------------------------------------
-- Expressions

instance (Show l) => Pretty (Exp l) where
  prettyPrec p (Lit _ l) = prettyPrec p l

  prettyPrec p (ClassLit _ mT) =
    ppResultType p mT <> text ".class"

  prettyPrec _ (This _) = text "this"

  prettyPrec p (QualifiedThis _ name) =
    prettyPrec p name <> text ".this"

  prettyPrec p (InstanceCreation _ tArgs tds args mBody) =
    hsep [text "new"
          , ppTypeParams p tArgs
          , prettyPrec p tds <> ppArgs args
         ] $$ maybePP p mBody

  prettyPrec p (QualInstanceCreation _ e tArgs ident args mBody) =
    hsep [prettyPrec p e <> char '.' <> text "new"
          , ppTypeParams p tArgs
          , prettyPrec p ident <> ppArgs args
         ] $$ maybePP p mBody

  prettyPrec p (ArrayCreate _ t es k) =
    text "new" <+>
      hcat (prettyPrec p t : map (brackets . prettyPrec p) es
                ++ replicate k (text "[]"))

  prettyPrec p (ArrayCreateInit _ t k ini) =
    text "new"
      <+> hcat (prettyPrec p t : replicate k (text "[]"))
      <+> prettyPrec p ini

  prettyPrec p (FieldAccess _ fa) = parenPrec p 1 $ prettyPrec 1 fa

  prettyPrec p (MethodInv _ mi) = parenPrec p 1 $ prettyPrec 1 mi

  prettyPrec p (ArrayAccess _ ain) = parenPrec p 1 $ prettyPrec 1 ain

  prettyPrec p (ExpName _ name) = prettyPrec p name

  prettyPrec p (PostIncrement _ e) = parenPrec p 1 $ prettyPrec 2 e <> text "++"

  prettyPrec p (PostDecrement _ e) = parenPrec p 1 $ prettyPrec 2 e <> text "--"

  prettyPrec p (PreIncrement _ e)  = parenPrec p 1 $ text "++" <> prettyPrec 2 e

  prettyPrec p (PreDecrement _ e)  = parenPrec p 1 $ text "--" <> prettyPrec 2 e

  prettyPrec p (PrePlus _ e) = parenPrec p 2 $ char '+' <> prettyPrec 2 e

  prettyPrec p (PreMinus _ e) = parenPrec p 2 $ char '-' <> prettyPrec 2 e

  prettyPrec p (PreBitCompl _ e) = parenPrec p 2 $ char '~' <> prettyPrec 2 e

  prettyPrec p (PreNot _ e) = parenPrec p 2 $ char '!' <> prettyPrec 2 e

  prettyPrec p (Cast _ t e) = parenPrec p 2 $ parens (prettyPrec p t) <+> prettyPrec 2 e

  prettyPrec p (BinOp _ e1 op e2) =
    let prec = opPrec op in
    parenPrec p prec (prettyPrec prec e1 <+> prettyPrec p op <+> prettyPrec prec e2)

  prettyPrec p (InstanceOf _ e rt) =
    let cp = opPrec LThan in
    parenPrec p cp $ prettyPrec cp e
                   <+> text "instanceof" <+> prettyPrec cp rt

  prettyPrec p (Cond _ c th el) =
    parenPrec p 13 $ prettyPrec 13 c <+> char '?'
                   <+> prettyPrec p th <+> colon <+> prettyPrec 13 el

  prettyPrec p (Assign _ lhs aop e) =
    hsep [prettyPrec p lhs, prettyPrec p aop, prettyPrec p e]

  prettyPrec p (Lambda _ params body) =
    prettyPrec p params <+> text "->" <+> prettyPrec p body

  prettyPrec p (MethodRef _ i1 i2) =
    prettyPrec p i1 <+> text "::" <+> prettyPrec p i2

instance (Show l) => Pretty (LambdaParams l) where
  prettyPrec p (LambdaSingleParam _ ident) = prettyPrec p ident
  prettyPrec _ (LambdaFormalParams _ params) = ppArgs params
  prettyPrec _ (LambdaInferredParams _ idents) = ppArgs idents

instance (Show l) => Pretty (LambdaExpression l) where
  prettyPrec p (LambdaExpression _ expression) = prettyPrec p expression
  prettyPrec p (LambdaBlock _ block) = prettyPrec p block

instance Pretty Literal where
  prettyPrec _ (Int i) = text (show i)
  prettyPrec _ (Word i) = text (show i) <> char 'L'
  prettyPrec _ (Float f) = text (show f) <> char 'F'
  prettyPrec _ (Double d) = text (show d)
  prettyPrec _ (Boolean b) = text . map toLower $ show b
  prettyPrec _ (Char c) = quotes $ text (escapeChar c)
  prettyPrec _ (String s) = doubleQuotes $ text (concatMap escapeString s)
  prettyPrec _ (Null) = text "null"

instance Pretty Op where
  prettyPrec _ op = text $ case op of
    Mult    -> "*"
    Div     -> "/"
    Rem     -> "%"
    Add     -> "+"
    Sub     -> "-"
    LShift  -> "<<"
    RShift  -> ">>"
    RRShift -> ">>>"
    LThan   -> "<"
    GThan   -> ">"
    LThanE  -> "<="
    GThanE  -> ">="
    Equal   -> "=="
    NotEq   -> "!="
    And     -> "&"
    Xor     -> "^"
    Or      -> "|"
    CAnd    -> "&&"
    COr     -> "||"

instance Pretty AssignOp where
  prettyPrec _ aop = text $ case aop of
    EqualA  -> "="
    MultA   -> "*="
    DivA    -> "/="
    RemA    -> "%="
    AddA    -> "+="
    SubA    -> "-="
    LShiftA -> "<<="
    RShiftA -> ">>="
    RRShiftA -> ">>>="
    AndA    -> "&="
    XorA    -> "^="
    OrA     -> "|="

instance (Show l) => Pretty (Lhs l) where
  prettyPrec p (NameLhs _ name) = prettyPrec p name
  prettyPrec p (FieldLhs _ fa) = prettyPrec p fa
  prettyPrec p (ArrayLhs _ ain) = prettyPrec p ain

instance (Show l) => Pretty (ArrayIndex l) where
  prettyPrec p (ArrayIndex _ ref e) = prettyPrec p ref <> hcat (map (brackets . prettyPrec p) e)

instance (Show l) => Pretty (FieldAccess l) where
  prettyPrec p (PrimaryFieldAccess _ e ident) =
    prettyPrec p e <> char '.' <> prettyPrec p ident
  prettyPrec p (SuperFieldAccess _ ident) =
    text "super." <> prettyPrec p ident
  prettyPrec p (ClassFieldAccess _ name ident) =
    prettyPrec p name <> text ".super." <> prettyPrec p ident

instance (Show l) => Pretty (MethodInvocation l) where
  prettyPrec p (MethodCall _ name args) =
    prettyPrec p name <> ppArgs args

  prettyPrec p (PrimaryMethodCall _ e tArgs ident args) =
    hcat [prettyPrec p e, char '.', ppTypeParams p tArgs,
           prettyPrec p ident, ppArgs args]

  prettyPrec p (SuperMethodCall _ tArgs ident args) =
    hcat [text "super.", ppTypeParams p tArgs,
           prettyPrec p ident, ppArgs args]

  prettyPrec p (ClassMethodCall _ name tArgs ident args) =
    hcat [prettyPrec p name, text ".super.", ppTypeParams p tArgs,
           prettyPrec p ident, ppArgs args]

  prettyPrec p (TypeMethodCall _ name tArgs ident args) =
    hcat [prettyPrec p name, char '.', ppTypeParams p tArgs,
           prettyPrec p ident, ppArgs args]

instance (Show l) => Pretty (ArrayInit l) where
  prettyPrec p (ArrayInit _ vInits) =
    braceBlock $ map (\v -> prettyPrec p v <> comma) vInits
    --braces $ hsep (punctuate comma (map (prettyPrec p) vInits))


ppArgs :: Pretty a => [a] -> Doc
ppArgs = parens . hsep . punctuate comma . map pretty

-----------------------------------------------------------------------
-- Types

instance Pretty Type where
  prettyPrec p (PrimType pt) = prettyPrec p pt
  prettyPrec p (RefType  rt) = prettyPrec p rt

instance Pretty RefType where
  prettyPrec p (ClassRefType ct) = prettyPrec p ct
  prettyPrec p (ArrayType t) = prettyPrec p t <> text "[]"

instance Pretty ClassType where
  prettyPrec p (WithPackage pkg name) =
    prettyPrec p pkg <> prettyPrec p name
  prettyPrec p (WithoutPackage name) =prettyPrec p name

instance Pretty ClassName where
  prettyPrec p (ClassName name) = hcat (punctuate (char '.') (map (\(i,tas) -> prettyPrec p i <> ppTypeParams p tas) name))
  prettyPrec p WildcardName = text "*"

instance Pretty Package where
  prettyPrec p (FullQualiPackage pkgs) = hcat ((punctuate (char '.') . map (prettyPrec p)) pkgs)
  prettyPrec p (WildcardPackage pkgs) = hcat ((punctuate (char '.') . map (prettyPrec p)) pkgs) <> text ".*"

instance Pretty TypeArgument where
  prettyPrec p (ActualType rt) = prettyPrec p rt
  prettyPrec p (Wildcard mBound) = char '?' <+> maybePP p mBound
  prettyPrec _ Diamond = text ""

instance Pretty TypeDeclSpecifier where
  prettyPrec p (TypeDeclSpecifier ct) = prettyPrec p ct

instance Pretty WildcardBound where
  prettyPrec p (ExtendsBound rt) = text "extends" <+> prettyPrec p rt
  prettyPrec p (SuperBound   rt) = text "super"   <+> prettyPrec p rt

instance Pretty (ExceptionType l) where
  prettyPrec p (ExceptionType _ rt) = prettyPrec p rt

instance Pretty PrimType where
  prettyPrec _ BooleanT = text "boolean"
  prettyPrec _ ByteT    = text "byte"
  prettyPrec _ ShortT   = text "short"
  prettyPrec _ IntT     = text "int"
  prettyPrec _ LongT    = text "long"
  prettyPrec _ CharT    = text "char"
  prettyPrec _ FloatT   = text "float"
  prettyPrec _ DoubleT  = text "double"

instance Pretty TypeParam where
  prettyPrec p (TypeParam ident rts) =
    prettyPrec p ident
      <+> opt (not $ null rts)
           (hsep $ text "extends":
                    punctuate (text " &") (map (prettyPrec p) rts))

ppTypeParams :: Pretty a => Int -> [a] -> Doc
ppTypeParams _ [] = empty
ppTypeParams p tps = char '<'
    <> hsep (punctuate comma (map (prettyPrec p) tps))
    <> char '>'

ppImplements :: Int -> [RefType] -> Doc
ppImplements _ [] = empty
ppImplements p rts = text "implements"
    <+> hsep (punctuate comma (map (prettyPrec p) rts))

ppExtends :: Int -> [RefType] -> Doc
ppExtends _ [] = empty
ppExtends p rts = text "extends"
    <+> hsep (punctuate comma (map (prettyPrec p) rts))

ppThrows :: Int -> [ExceptionType l] -> Doc
ppThrows _ [] = empty
ppThrows p ets = text "throws"
    <+> hsep (punctuate comma (map (prettyPrec p) ets))

ppDefault :: (Show l) => Int -> Maybe (Exp l) -> Doc
ppDefault _ Nothing = empty
ppDefault p (Just expression) = text "default" <+> prettyPrec p expression

ppResultType :: Int -> Maybe Type -> Doc
ppResultType _ Nothing = text "void"
ppResultType p (Just a) = prettyPrec p a

-----------------------------------------------------------------------
-- Names and identifiers

instance Pretty Name where
  prettyPrec p (Name is) =
    hcat (punctuate (char '.') $ map (prettyPrec p) is)

instance Pretty Ident where
  prettyPrec _ (Ident s) = text s


-----------------------------------------------------------------------
-- Help functionality
prettyNestedStmt :: (Show l) => Int -> Stmt l -> Doc
prettyNestedStmt prio p@(StmtBlock _ _) = prettyPrec prio p
prettyNestedStmt prio p = nest 2 (prettyPrec prio p)

maybePP :: Pretty a => Int -> Maybe a -> Doc
maybePP p = maybe empty (prettyPrec p)

opt :: Bool -> Doc -> Doc
opt x a = if x then a else empty

braceBlock :: [Doc] -> Doc
braceBlock xs = char '{'
    $+$ nest 2 (vcat xs)
    $+$ char '}'

opPrec Mult    = 3
opPrec Div     = 3
opPrec Rem     = 3
opPrec Add     = 4
opPrec Sub     = 4
opPrec LShift  = 5
opPrec RShift  = 5
opPrec RRShift = 5
opPrec LThan   = 6
opPrec GThan   = 6
opPrec LThanE  = 6
opPrec GThanE  = 6
opPrec Equal   = 7
opPrec NotEq   = 7
opPrec And     = 8
opPrec Xor     = 9
opPrec Or      = 10
opPrec CAnd    = 11
opPrec COr     = 12

escapeGeneral :: Char -> String
escapeGeneral '\b' = "\\b"
escapeGeneral '\t' = "\\t"
escapeGeneral '\n' = "\\n"
escapeGeneral '\f' = "\\f"
escapeGeneral '\r' = "\\r"
escapeGeneral '\\' = "\\\\"
escapeGeneral c | c >= ' ' && c < '\DEL' = [c]
                | c <= '\xFFFF' = printf "\\u%04x" (fromEnum c)
                | otherwise = error $ "Language.Java.Pretty.escapeGeneral: Char " ++ show c ++ " too large for Java char"

escapeChar :: Char -> String
escapeChar '\'' = "\\'"
escapeChar c = escapeGeneral c

escapeString :: Char -> String
escapeString '"' = "\\\""
escapeString c | c <= '\xFFFF' = escapeGeneral c
               | otherwise = escapeGeneral lead ++ escapeGeneral trail
                   where c' = fromEnum c - 0x010000
                         lead = toEnum $ 0xD800 + c' `div` 0x0400
                         trail = toEnum $ 0xDC00 + c' `mod` 0x0400

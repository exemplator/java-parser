module Language.Java.Helper
    ( getBody
    , desugarAnnotation
    , desugarAnnotation'
    ) where

import           Language.Java.Syntax

-----------------------------------------------------------------------
-- Functions

desugarAnnotation :: Annotation l -> (Name, [(Ident, ElementValue l)])
desugarAnnotation (MarkerAnnotation n)          = (n, [])
desugarAnnotation (SingleElementAnnotation n e) = (n, [(Ident "value", e)])
desugarAnnotation (NormalAnnotation n kv)       = (n, kv)
desugarAnnotation' :: Annotation l -> Annotation l
desugarAnnotation' = uncurry NormalAnnotation . desugarAnnotation

getBody :: TypeDecl l -> [Decl l]
getBody (ClassTypeDecl _ (ClassDecl _ _ _ _ _ _ (ClassBody _ decls))) = decls
getBody (ClassTypeDecl _ (EnumDecl _ _ _ _ (EnumBody _ _ decls))) = decls
getBody (InterfaceTypeDecl l (InterfaceDecl _ _ _ _ _ _ (InterfaceBody _ memDecls))) = map (MemberDecl l) memDecls

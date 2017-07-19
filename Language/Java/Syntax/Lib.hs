module Language.Java.Syntax.Lib
    ( getBody
    ) where

import           Language.Java.Syntax

-----------------------------------------------------------------------
-- Functions

getBody :: TypeDecl l -> [Decl l]
getBody (ClassTypeDecl _ (ClassDecl _ _ _ _ _ _ (ClassBody _ decls))) = decls
getBody (ClassTypeDecl _ (EnumDecl _ _ _ _ (EnumBody _ _ decls))) = decls
getBody (InterfaceTypeDecl l (InterfaceDecl _ _ _ _ _ _ (InterfaceBody _ memDecls))) = map (MemberDecl l) memDecls
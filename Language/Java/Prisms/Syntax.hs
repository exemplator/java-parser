{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Prisms.Syntax where


import           Control.Lens
import           Language.Java.Syntax

makeLenses ''CompilationUnit

makeLenses ''PackageDecl

makeLenses ''ImportDecl

makeLenses ''TypeDecl

makeLenses ''ClassDecl

makeLenses ''ClassBody

makeLenses ''EnumBody

makeLenses ''EnumConstant

makeLenses ''InterfaceDecl

makeLenses ''InterfaceBody

makeLenses ''InterfaceKind

makeLenses ''Decl

makeLenses ''MemberDecl

makeLenses ''VarDecl

makeLenses ''VarDeclId

makeLenses ''VarInit

makeLenses ''FormalParam

makeLenses ''MethodBody

makeLenses ''ConstructorBody

makeLenses ''ExplConstrInv

makeLenses ''Modifier

makeLenses ''Annotation

makeLenses ''FormalParam

makeLenses ''ElementValue

makeLenses ''Block

makeLenses ''BlockStmt

makeLenses ''Stmt

makeLenses ''Catch

makeLenses ''SwitchBlock

makeLenses ''SwitchLabel

makeLenses ''ForInit

makeLenses ''ExceptionType

makeLenses ''Exp

makeLenses ''Lhs

makeLenses ''ArrayIndex

makeLenses ''FieldAccess

makeLenses ''LambdaParams

makeLenses ''LambdaExpression

makeLenses ''ArrayInit

makeLenses ''MethodInvocation

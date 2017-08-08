{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Prisms.Syntax where


import           Language.Java.Prisms.CustomLenses
import qualified Language.Java.Syntax              as S

allLenses ''S.CompilationUnit

allLenses ''S.PackageDecl

allLenses ''S.ImportDecl

allLenses ''S.TypeDecl

allLenses ''S.ClassDecl

allLenses ''S.ClassBody

allLenses ''S.EnumBody

allLenses ''S.EnumConstant

allLenses ''S.InterfaceDecl

allLenses ''S.InterfaceBody

allLenses ''S.InterfaceKind

allLenses ''S.Decl

allLenses ''S.MemberDecl

allLenses ''S.VarDecl

allLenses ''S.VarDeclId

allLenses ''S.VarInit

allLenses ''S.FormalParam

allLenses ''S.MethodBody

allLenses ''S.ConstructorBody

allLenses ''S.ExplConstrInv

allLenses ''S.Modifier

allLenses ''S.Annotation

allLenses ''S.ElementValue

allLenses ''S.Block

allLenses ''S.BlockStmt

allLenses ''S.Stmt

allLenses ''S.Catch

allLenses ''S.SwitchBlock

allLenses ''S.SwitchLabel

allLenses ''S.ForInit

allLenses ''S.ExceptionType

allLenses ''S.Exp

allLenses ''S.Lhs

allLenses ''S.ArrayIndex

allLenses ''S.FieldAccess

allLenses ''S.LambdaParams

allLenses ''S.LambdaExpression

allLenses ''S.ArrayInit

allLenses ''S.MethodInvocation

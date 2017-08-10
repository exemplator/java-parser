{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Lenses.Syntax where


import           Language.Java.Lenses.CustomLenses
import qualified Language.Java.Syntax              as S

allLenses ''S.CompilationUnitNode
allLenses ''S.ModuleSpecNode
allLenses ''S.TypeDeclNode
allLenses ''S.ClassDeclNode
allLenses ''S.DeclNode
allLenses ''S.MemberDeclNode
allLenses ''S.VarDeclIdNode
allLenses ''S.VarInitNode
allLenses ''S.ExplConstrInvNode
allLenses ''S.BlockStmtNode
allLenses ''S.StmtNode
allLenses ''S.TryResourceNode
allLenses ''S.SwitchLabelNode
allLenses ''S.ForInitNode
allLenses ''S.ExpNode
allLenses ''S.LhsNode
allLenses ''S.FieldAccessNode
allLenses ''S.LambdaParamsNode
allLenses ''S.LambdaExpressionNode
allLenses ''S.MethodInvocationNode
allLenses ''S.CompilationUnit
allLenses ''S.ModuleDeclaration
allLenses ''S.PackageDecl
allLenses ''S.ModuleRequires
allLenses ''S.ModuleExports
allLenses ''S.ImportDecl
allLenses ''S.ClassTypeDecl
allLenses ''S.InterfaceTypeDecl
allLenses ''S.ClassDecl
allLenses ''S.EnumDecl
allLenses ''S.Extends
allLenses ''S.Implements
allLenses ''S.ClassBody
allLenses ''S.EnumBody
allLenses ''S.EnumConstant
allLenses ''S.InterfaceDecl
allLenses ''S.InterfaceKind
allLenses ''S.InterfaceBody
allLenses ''S.MemberDecl
allLenses ''S.InitDecl
allLenses ''S.FieldDecl
allLenses ''S.MethodDecl
allLenses ''S.ConstructorDecl
allLenses ''S.MemberClassDecl
allLenses ''S.MemberInterfaceDecl
allLenses ''S.VarDecl
allLenses ''S.VarId
allLenses ''S.VarDeclArray
allLenses ''S.FormalParam
allLenses ''S.MethodBody
allLenses ''S.ConstructorBody
allLenses ''S.ThisInvoke
allLenses ''S.SuperInvoke
allLenses ''S.PrimarySuperInvoke
allLenses ''S.Modifier
allLenses ''S.Annotation
allLenses ''S.ElementValue
allLenses ''S.Block
allLenses ''S.BlockStmt
allLenses ''S.LocalClass
allLenses ''S.LocalVars
allLenses ''S.StmtBlock
allLenses ''S.IfThenElse
allLenses ''S.While
allLenses ''S.BasicFor
allLenses ''S.EnhancedFor
allLenses ''S.Empty
allLenses ''S.ExpStmt
allLenses ''S.Assert
allLenses ''S.Switch
allLenses ''S.Do
allLenses ''S.Break
allLenses ''S.Continue
allLenses ''S.Return
allLenses ''S.Synchronized
allLenses ''S.Throw
allLenses ''S.Try
allLenses ''S.Labeled
allLenses ''S.Catch
allLenses ''S.TryResourceVar
allLenses ''S.TryResourceFinalVar
allLenses ''S.SwitchBlock
allLenses ''S.SwitchCase
allLenses ''S.SwitchDefault
allLenses ''S.ForLocalVars
allLenses ''S.ForInitExps
allLenses ''S.ExceptionType
allLenses ''S.Lit
allLenses ''S.ClassLit
allLenses ''S.This
allLenses ''S.QualifiedThis
allLenses ''S.InstanceCreation
allLenses ''S.QualInstanceCreation
allLenses ''S.ArrayCreate
allLenses ''S.ArrayCreateInit
allLenses ''S.FieldAccess
allLenses ''S.MethodInv
allLenses ''S.ArrayAccess
allLenses ''S.ExpName
allLenses ''S.PostIncrement
allLenses ''S.PostDecrement
allLenses ''S.PreIncrement
allLenses ''S.PreDecrement
allLenses ''S.PrePlus
allLenses ''S.PreMinus
allLenses ''S.PreBitCompl
allLenses ''S.PreNot
allLenses ''S.Cast
allLenses ''S.BinOp
allLenses ''S.InstanceOf
allLenses ''S.Cond
allLenses ''S.Assign
allLenses ''S.Lambda
allLenses ''S.MethodRef
allLenses ''S.NameLhs
allLenses ''S.FieldLhs
allLenses ''S.ArrayLhs
allLenses ''S.ArrayIndex
allLenses ''S.PrimaryFieldAccess
allLenses ''S.SuperFieldAccess
allLenses ''S.ClassFieldAccess
allLenses ''S.LambdaSingleParam
allLenses ''S.LambdaFormalParams
allLenses ''S.LambdaInferredParams
allLenses ''S.LambdaExpression
allLenses ''S.LambdaBlock
allLenses ''S.MethodCall
allLenses ''S.PrimaryMethodCall
allLenses ''S.SuperMethodCall
allLenses ''S.ClassMethodCall
allLenses ''S.TypeMethodCall
allLenses ''S.ArrayInit

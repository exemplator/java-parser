{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Java.SyntaxClasses where

import           Data.Function        (on)
import           Language.Java.Syntax

-- | Provides functionality to access the body as a list of declarations of a class, enum and an interface.
class HasBody a l where
  getBody :: a -> [DeclNode l]

-- | Get type of TypeDeclNode
instance HasType (TypeDeclNode l) where
  getType (ClassTypeDeclNode _ ctd) = getType ctd
  getType (InterfaceTypeDeclNode _ itd) = getType itd

instance CollectTypes (TypeDeclNode l) where
  collectTypes (ClassTypeDeclNode _ ctd) = collectTypes ctd
  collectTypes (InterfaceTypeDeclNode _ itd) = collectTypes itd

-- | Get the body of TypeDecl
instance HasBody (TypeDeclNode l) l where
  getBody (ClassTypeDeclNode _ classDeclB) = getBody classDeclB
  getBody (InterfaceTypeDeclNode _ iterDecl) = getBody iterDecl

-- | Get type of ClassDecl
instance HasType (ClassDeclNode l) where
  getType (ClassDeclNode _ ctd) = getType ctd
  getType (EnumDeclNode _ itd) = getType itd
  --getType (ClassDeclNode _ _ i _ _ _ _) = withoutPackageIdentToType i
  --getType (EnumDeclNode _ _ i _ _) = withoutPackageIdentToType i

-- | Get the body of ClassDecl
instance HasBody (ClassDeclNode l) l where
  getBody (ClassDeclNode _ ctd) = getBody ctd
  getBody (EnumDeclNode _ itd) = getBody itd
  --getBody (ClassDeclNode _ _ _ _ _ _ classBodyB) = getBody classBodyB
  --getBody (EnumDeclNode _ _ _ _ enumBodyB) = getBody enumBodyB

instance CollectTypes (ClassDeclNode l) where
  collectTypes (ClassDeclNode _ ctd) = collectTypes ctd
  collectTypes (EnumDeclNode _ itd) = collectTypes itd
  --collectTypes (ClassDeclNode _ _ i _ _ types _) = withoutPackageIdentToType i : collectTypes types
  --collectTypes (EnumDeclNode _ _ i types _) = withoutPackageIdentToType i : collectTypes types

-- | Get type of MemberDecl if it is a MethodDecl (our solution to handeling the Maybe)
instance CollectTypes (MemberDeclNode l) where
  collectTypes (FieldDeclNode _ ctd) = collectTypes ctd
  collectTypes (MethodDeclNode _ ctd) = collectTypes ctd
  collectTypes (ConstructorDeclNode _ ctd) = []
  collectTypes (MemberClassDeclNode _ ctd) = collectTypes ctd
  collectTypes (MemberClassDeclNode _ ctd) = collectTypes ctd
  collectTypes (MemberInterfaceDeclNode _ ctd) = collectTypes ctd

instance Eq l => Ord (MemberDeclNode l) where
  compare = compare `on` memToInt
    where
      memToInt FieldDeclNode{} = 1
      memToInt MethodDeclNode{} = 2
      memToInt ConstructorDeclNode{} = 3
      memToInt MemberClassDeclNode{} = 4
      memToInt MemberInterfaceDeclNode{} = 5

instance HasType (ImportDecl l) where
  getType = getTypeFromPackage . importPackage

getTypeFromPackage :: Package -> Type
getTypeFromPackage pkg = RefType $ ClassRefType $ WithPackage pkg WildcardName

-- TODO ClassTypeDecl InterfaceTypeDecl

instance HasType (ClassDecl l) where
  getType (ClassDecl _ _ i _ _ _ _) = withoutPackageIdentToType i
instance HasType (EnumDecl l) where
  getType (EnumDecl _ _ i _ _) = withoutPackageIdentToType i
-- | Get the body of ClassDecl
instance HasBody (ClassDecl l) l where
  getBody (ClassDecl _ _ _ _ _ _ classBodyB) = getBody classBodyB
instance HasBody (EnumDecl l) l where
  getBody (EnumDecl _ _ _ _ enumBodyB) = getBody enumBodyB

instance CollectTypes (ClassDecl l) where
  collectTypes (ClassDecl _ _ i _ _ types _) = withoutPackageIdentToType i : collectTypes types
instance CollectTypes (EnumDecl l) where
  collectTypes (EnumDecl _ _ i types _) = withoutPackageIdentToType i : collectTypes types

instance HasType (Extends l) where
  getType = getType . extendsClass

instance HasType (Implements l) where
  getType = getType . implementsInterface

-- | Gets type of FormalParam
instance HasType (FormalParam l) where
  getType (FormalParam _ _ t _ _) =  t

-- | Get the body of ClassBody
instance HasBody (ClassBody l) l where
  getBody (ClassBody _ decls) = decls


-- | Get the body of EnumBody
instance HasBody (EnumBody l) l where
  getBody (EnumBody _ _ decls) = decls


-- | Get type of EnumConstant
instance HasType (EnumConstant l) where
  getType (EnumConstant _ i _ _) =  withoutPackageIdentToType i


-- | Get type of InterfaceDecl
instance HasType (InterfaceDecl l) where
  getType (InterfaceDecl _ _ _ i _ _ _) =  withoutPackageIdentToType i

instance CollectTypes (InterfaceDecl l) where
  collectTypes (InterfaceDecl _ _ _ i _ types _) = withoutPackageIdentToType i : collectTypes types

-- | Get the body of InterfaceDecl
instance HasBody (InterfaceDecl l) l where
  getBody (InterfaceDecl _ _ _ _ _ _ iterBody) = getBody iterBody


-- | Get the body of ClassDecl
instance HasBody (InterfaceBody l) l where
  getBody (InterfaceBody l memDecls) = map (MemberDeclNode l) memDecls


-- | Get type of MemberDecl if it is a MethodDecl (our solution to handeling the Maybe)
instance CollectTypes (FieldDecl l) where
  collectTypes (FieldDecl _ _ t _) =  [t]
instance CollectTypes (MethodDecl l) where
  collectTypes (MethodDecl _ _ _ _ name _ _ _ _) =  [withoutPackageIdentToType name]

--- HasNodes

class HasNode el no where
  toNode :: (el l) -> (no l)

-----------------------------------------------------------------------
-- Nodes

instance HasNode CompilationUnit CompilationUnitNode where
  toNode = CompilationUnitNode
instance HasNode ModuleDeclaration CompilationUnitNode where
  toNode = ModuleDeclarationNode

instance HasNode ModuleRequires ModuleSpecNode where
  toNode = ModuleRequiresNode
instance HasNode ModuleExports ModuleSpecNode where
  toNode = ModuleRequiresNode

instance HasNode ClassDeclNode TypeDeclNode where
  toNode = ClassTypeDeclNode
instance HasNode InterfaceDecl TypeDeclNode where
  toNode = InterfaceTypeDeclNode

instance HasNode ClassDecl ClassDeclNode where
  toNode = ClassDeclNode
instance HasNode EnumDecl ClassDeclNode where
  toNode = ClassDeclNode

instance HasNode MemberDeclNode DeclNode where
  toNode = MemberDeclNode
instance HasNode InitDecl DeclNode where
  toNode = InitDeclNode

instance HasNode FieldDecl MemberDeclNode where
  toNode = FieldDeclNode
instance HasNode MethodDecl MemberDeclNode where
  toNode = MethodDeclNode
instance HasNode ConstructorDecl MemberDeclNode where
  toNode = ConstructorDeclNode
instance HasNode ClassDecl MemberDeclNode where
  toNode = MemberClassDeclNode
instance HasNode InterfaceDecl MemberDeclNode where
  toNode = MemberInterfaceDeclNode

instance HasNode VarId VarDeclIdNode where
  toNode = VarIdNode
instance HasNode VarDeclArray VarDeclIdNode where
  toNode = VarDeclArrayNode
instance HasNode ExpNode VarInitNode where
  toNode = InitExpNode
instance HasNode ArrayInit VarInitNode where
  toNode = InitArrayNode

instance HasNode ThisInvoke ExplConstrInvNode where
  toNode = ThisInvokeNode
instance HasNode SuperInvoke ExplConstrInvNode where
  toNode = SuperInvokeNode
instance HasNode ExpNode ExplConstrInvNode where
  toNode = PrimarySuperInvoke

instance HasNode StmtNode BlockStmtNode where
  toNode = BlockStmtNode
instance HasNode ClassDecl BlockStmtNode where
  toNode = LocalClassNode
instance HasNode LocalVars BlockStmtNode where
  toNode = LocalVarsNode

instance HasNode Block StmtNode where
  toNode = StmtBlockNode
instance HasNode IfThenElse StmtNode where
  toNode = IfThenElseNode
instance HasNode While StmtNode where
  toNode = WhileNode
instance HasNode BasicFor StmtNode where
  toNode = BasicForNode
instance HasNode EnhancedFor StmtNode where
  toNode = EnhancedForNode
instance HasNode Empty StmtNode where
  toNode = EmptyNode
instance HasNode ExpNode StmtNode where
  toNode = ExpStmtNode
instance HasNode Assert StmtNode where
  toNode = AssertNode
instance HasNode Switch StmtNode where
  toNode = SwitchNode
instance HasNode Do StmtNode where
  toNode = DoNode
instance HasNode Break StmtNode where
  toNode = BreakNode
instance HasNode Continue StmtNode where
  toNode = ContinueNode
instance HasNode Return StmtNode where
  toNode = ReturnNode
instance HasNode Synchronized StmtNode where
  toNode = SynchronizedNode
instance HasNode Throw StmtNode where
  toNode = ThrowNode
instance HasNode Try StmtNode where
  toNode = TryNode
instance HasNode Labeled StmtNode where
  toNode = LabeledNode

instance HasNode TryResourceVar TryResourceNode where
  toNode = TryResourceVarNode
instance HasNode TryResourceFinalVar TryResourceNode where
  toNode = TryResourceFinalVarNode

instance HasNode ExpNode SwitchLabelNode where
  toNode = SwitchCaseNode

instance HasNode ForLocalVars ForInitNode where
  toNode = ForLocalVarsNode
instance HasNode ForInitExps ForInitNode where
  toNode = ForInitExpsNode

instance HasNode Lit ExpNode where
  toNode = LitNode
instance HasNode ClassLit ExpNode where
  toNode = ClassLitNode
instance HasNode This ExpNode where
  toNode = ThisNode
instance HasNode QualifiedThis ExpNode where
  toNode = QualifiedThisNode
instance HasNode InstanceCreation ExpNode where
  toNode = InstanceCreationNode
instance HasNode QualInstanceCreation ExpNode where
  toNode = QualInstanceCreationNode
instance HasNode ArrayCreate ExpNode where
  toNode = ArrayCreateNode
instance HasNode ArrayCreateInit ExpNode where
  toNode = ArrayCreateInitNode
instance HasNode FieldAccessNode ExpNode where
  toNode = FieldAccessNode
instance HasNode MethodInv ExpNode where
  toNode = MethodInvNode
instance HasNode ArrayAccess ExpNode where
  toNode = ArrayAccessNode
instance HasNode ExpName ExpNode where
  toNode = ExpNameNode
instance HasNode ExpNode ExpNode where
  toNode = PreNotNode
instance HasNode Cast ExpNode where
  toNode = CastNode
instance HasNode BinOp ExpNode where
  toNode = BinOpNode
instance HasNode InstanceOf ExpNode where
  toNode = InstanceOfNode
instance HasNode Cond ExpNode where
  toNode = CondNode
instance HasNode Assign ExpNode where
  toNode = AssignNode
instance HasNode Lambda ExpNode where
  toNode = LambdaNode
instance HasNode MethodRef ExpNode where
  toNode = MethodRefNode

instance HasNode NameLhs LhsNode where
  toNode = NameLhsNode
instance HasNode FieldAccessNode LhsNode where
  toNode = FieldLhsNode
instance HasNode ArrayIndex LhsNode where
  toNode = ArrayLhsNode

instance HasNode PrimaryFieldAccess FieldAccessNode where
  toNode = PrimaryFieldAccessNode
instance HasNode SuperFieldAccess FieldAccessNode where
  toNode = SuperFieldAccessNode
instance HasNode ClassFieldAccess FieldAccessNode where
  toNode = ClassFieldAccessNode

instance HasNode LambdaSingleParam LambdaParamsNode where
  toNode = LambdaSingleParamNode
instance HasNode LambdaFormalParams LambdaParamsNode where
  toNode = LambdaFormalParamsNode
instance HasNode LambdaInferredParams LambdaParamsNode where
  toNode = LambdaInferredParamsNode

instance HasNode ExpNode LambdaExpressionNode where
  toNode = LambdaExpressionNode
instance HasNode Block LambdaExpressionNode where
  toNode = LambdaBlockNode

instance HasNode MethodCall MethodInvocationNode where
  toNode = MethodCallNode
instance HasNode PrimaryMethodCall MethodInvocationNode where
  toNode = PrimaryMethodCallNode
instance HasNode SuperMethodCall MethodInvocationNode where
  toNode = SuperMethodCallNode
instance HasNode ClassMethodCall MethodInvocationNode where
  toNode = ClassMethodCallNode
instance HasNode TypeMethodCall MethodInvocationNode where
  toNode = TypeMethodCallNode


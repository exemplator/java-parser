{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Language.Java.Syntax(
  CompilationUnitNodesNode (..),
  ModuleSpecNode (..),
  TypeDeclNode (..),
  ClassDeclNode (..),
  DeclNode (..),
  MemberDeclNode (..),
  VarDeclIdNode (..),
  VarInitNode (..),
  ExplConstrInvNode (..),
  BlockStmtNode (..),
  StmtNode (..),
  TryResourceNode (..),
  SwitchLabelNode (..),
  ForInitNode (..),
  ExpNode (..),
  LhsNode (..),
  FieldAccessNode (..),
  LambdaParamsNode (..),
  LambdaExpressionNode (..),
  MethodInvocationNode (..),
  CompilationUnit (..),
  ModuleDeclaration (..),
  PackageDecl (..),
  ModuleRequires (..),
  ModuleExports (..),
  ImportDecl (..),
  ClassTypeDecl (..),
  InterfaceTypeDecl (..),
  ClassDecl (..),
  EnumDecl (..),
  Extends (..),
  Implements (..),
  ClassBody (..),
  EnumBody (..),
  EnumConstant (..),
  InterfaceDecl (..),
  InterfaceKind (..),
  InterfaceBody (..),
  Decl (..),
  InitDecl (..),
  FieldDecl (..),
  MethodDecl (..),
  ConstructorDecl (..),
  MemberClassDecl (..),
  MemberInterfaceDecl (..),
  VarDecl (..),
  VarId (..),
  VarDeclArray (..),
  InitExp (..),
  InitArray (..),
  FormalParam (..),
  MethodBody (..),
  ConstructorBody (..),
  ThisInvoke (..),
  SuperInvoke (..),
  PrimarySuperInvoke (..),
  Modifier (..),
  Annotation (..),
  ElementValue (..),
  Block (..),
  BlockStmt (..),
  LocalClass (..),
  LocalVars (..),
  StmtBlock (..),
  IfThenElse (..),
  While (..),
  BasicFor (..),
  EnhancedFor (..),
  ExpStmt (..),
  Assert (..),
  Switch (..),
  Do (..),
  Break (..),
  Continue (..),
  Return (..),
  Synchronized (..),
  Throw (..),
  Try (..),
  Labeled (..),
  Catch (..),
  TryResourceVar (..),
  TryResourceFinalVar (..),
  SwitchBlock (..),
  SwitchCase (..),
  ForLocalVars (..),
  ForInitExps (..),
  ExceptionType (..),
  Lit (..),
  ClassLit (..),
  QualifiedThis (..),
  InstanceCreation (..),
  QualInstanceCreation (..),
  ArrayCreate (..),
  ArrayCreateInit (..),
  FieldAccess (..),
  MethodInv (..),
  ArrayAccess (..),
  ExpName (..),
  PostIncrement (..),
  PostDecrement (..),
  PreIncrement (..),
  PreDecrement (..),
  PrePlus (..),
  PreMinus (..),
  PreBitCompl (..),
  PreNot (..),
  Cast (..),
  BinOp (..),
  InstanceOf (..),
  Cond (..),
  Assign (..),
  Lambda (..),
  MethodRef (..),
  NameLhs (..),
  FieldLhs (..),
  ArrayLhs (..),
  ArrayIndex (..),
  PrimaryFieldAccess (..),
  SuperFieldAccess (..),
  ClassFieldAccess (..),
  LambdaSingleParam (..),
  LambdaFormalParams (..),
  LambdaInferredParams (..),
  LambdaExpression (..),
  LambdaBlock (..),
  MethodCall (..),
  PrimaryMethodCall (..),
  SuperMethodCall (..),
  ClassMethodCall (..),
  TypeMethodCall (..),
  ArrayInit (..),
  module Language.Java.Syntax.Types,
  module Language.Java.Syntax.Exp
) where

import           Data.Data
import           GHC.Generics               (Generic)

import           Language.Java.Syntax.Exp
import           Language.Java.Syntax.Types

-----------------------------------------------------------------------
-- Nodes

-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnitNodesNode l
  = CompilationUnitNode l (CompilationUnit l)
  | ModuleDeclarationNode l (ModuleDeclaration l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | specifies the module declarations
data ModuleSpecNode l
  -- | requires the module to work
  = ModuleRequiresNode l (ModuleRequires l)
  -- | exports the package
  | ModuleExportsNode l (ModuleExports l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-----------------------------------------------------------------------
-- Declarations


-- | A type declaration declares a class type or an interface type.
data TypeDeclNode l
    = ClassTypeDeclNode l (ClassTypeDecl l)
    | InterfaceTypeDeclNode l (InterfaceTypeDecl l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A class declaration specifies a new named reference type.
data ClassDeclNode l
    = ClassDeclNode l (ClassDecl l)
    | EnumDeclNode l (EnumDecl l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data DeclNode l
    = MemberDeclNode l (MemberDeclNode l)
    | InitDeclNode l (InitDecl l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDeclNode l
    -- | The variables of a class type are introduced by field declarations.
    = FieldDeclNode l (FieldDecl l)
    -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    | MethodDeclNode l (MethodDecl l)
    -- | A constructor is used in the creation of an object that is an instance of a class.
    | ConstructorDeclNode l (ConstructorDecl l)
    -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    | MemberClassDeclNode l (MemberClassDecl l)
    -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    | MemberInterfaceDeclNode l (MemberInterfaceDecl l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The name of a variable in a declaration, which may be an array.
data VarDeclIdNode l
    = VarIdNode l (VarId l)
    | VarDeclArrayNode l (VarDeclArray l)
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Explicit initializer for a variable declaration.
data VarInitNode l
    = InitExpNode l (InitExp l)
    | InitArrayNode l (InitArray l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInvNode l
    = ThisInvokeNode l (ThisInvoke l)
    | SuperInvokeNode l (SuperInvoke l)
    | PrimarySuperInvokeNode l (PrimarySuperInvoke l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-----------------------------------------------------------------------
-- Statements

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmtNode l
    = BlockStmtNode l (BlockStmt l)
    | LocalClassNode l (LocalClass l)
    | LocalVarsNode l (LocalVars l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | A Java statement.
data StmtNode l
    -- | A statement can be a nested block.
    = StmtBlockNode l (StmtBlock l)
    -- | The @if-then@ statement allows conditional execution of a statement.
    | IfThenElseNode l (IfThenElse l)
    -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    | WhileNode l (While l)
    -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    | BasicForNode l (BasicFor l)
    -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    | EnhancedForNode l (EnhancedFor l)
    -- | An empty statement does nothing.
    | EmptyNode l (Empty l)
    -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    | ExpStmtNode l (ExpStmt l)
    -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    | AssertNode l (Assert l)
    -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    | SwitchNode l (Switch l)
    -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    | DoNode l (Do l)
    -- | A @break@ statement transfers control out of an enclosing statement.
    | BreakNode l (Break l)
    -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    | ContinueNode l (Continue l)
    -- A @return@ statement returns control to the invoker of a method or constructor.
    | ReturnNode l (Return l)
    -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    | SynchronizedNode l (Synchronized l)
    -- | A @throw@ statement causes an exception to be thrown.
    | ThrowNode l (Throw l)
    -- | A try statement executes a block and may catch a thrown exception
    | TryNode l (Try l)
    -- | Statements may have label prefixes.
    | LabeledNode l (Labeled l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Resource in a try-with-resources statement
data TryResourceNode l
    -- | Newly declared variables
    = TryResourceVarNode l (TryResourceVar l)
    -- | Effectively final variable
    | TryResourceFinalVarNode l (TryResourceFinalVar l)
    deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A label within a @switch@ statement.
data SwitchLabelNode l
    -- | The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
    = SwitchCaseNode l (SwitchCase l)
    | DefaultNode l (SwitchDefault l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Initialization code for a basic @for@ statement.
data ForInitNode l
    = ForLocalVarsNode l (ForLocalVars l)
    | ForInitExpsNode l (ForInitExps l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A Java expression.
data ExpNode l
    -- | A literal denotes a fixed, unchanging value.
    = LitNode l (Lit l)
    -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    | ClassLitNode l (ClassLit l)
    -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    | ThisNode l (This l)
    -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    -- TODO: Fix Parser here
    | QualifiedThisNode l (QualifiedThis l)
    -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    | InstanceCreationNode l (InstanceCreation l)
    -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    {- TODO what is is the mysteryExp used for?-}
    | QualInstanceCreationNode l (QualInstanceCreation l)
    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    | ArrayCreateNode l (ArrayCreate l)
    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    | ArrayCreateInitNode l (ArrayCreateInit l)
    -- | A field access expression.
    | FieldAccessNode l (FieldAccess l)
    -- | A method invocation expression.
    | MethodInvNode l (MethodInv l)
    -- | An array access expression refers to a variable that is a component of an array.
    | ArrayAccessNode l (ArrayAccess l)
{-    | ArrayAccess ExpNode ExpNode -- Should this be made into a datatype, for consistency and use with Lhs? -}
    -- | An expression name, e.g. a variable.
    | ExpNameNode l (ExpName l)
    -- | Post-incrementation expression, i.e. an expression followed by @++@.
    | PostIncrementNode l (PostIncrement l)
    -- | Post-decrementation expression, i.e. an expression followed by @--@.
    | PostDecrementNode l (PostDecrement l)
    -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    | PreIncrementNode l (PreIncrement l)
    -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    | PreDecrementNode l (PreDecrement l)
    -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    | PrePlusNode l (PrePlus l)
    -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    | PreMinusNode l (PreMinus l)
    -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    | PreBitComplNode l (PreBitCompl l)
    -- | Logical complementation of boolean values.
    | PreNotNode l (PreNot l)
    -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    | CastNode l (Cast l)
    -- | The application of a binary operator to two operand expressions.
    | BinOpNode l (BinOp l)
    -- | Testing whether the result of an expression is an instance of some reference type.
    | InstanceOfNode l (InstanceOf l)
    -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    | CondNode l (Cond l)
    -- | Assignment of the result of an expression to a variable.
    | AssignNode l (Assign l)
    -- | Lambda expression
    | LambdaNode l (Lambda l)
    -- | Method reference
    | MethodRefNode l (MethodRef l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data LhsNode l
    = NameLhsNode l (NameLhs l)         -- ^ Assign to a variable
    | FieldLhsNode l (FieldLhs l)  -- ^ Assign through a field access
    | ArrayLhsNode l (ArrayLhs l)   -- ^ Assign to an array
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccessNode l
    = PrimaryFieldAccessNode l (PrimaryFieldAccess l) -- ^ Accessing a field of an object or array computed from an expression.
    | SuperFieldAccessNode l (SuperFieldAccess l) -- ^ Accessing a field of the superclass.
    | ClassFieldAccessNode l (ClassFieldAccess l) -- ^ Accessing a (static) field of a named class.
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaParamsNode l
  = LambdaSingleParamNode l (LambdaSingleParam l)
  | LambdaFormalParamsNode l (LambdaFormalParams l)
  | LambdaInferredParamsNode l (LambdaInferredParams l)
    deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Lambda expression, starting from java 8
data LambdaExpressionNode l
    = LambdaExpressionNode l (LambdaExpression l)
    | LambdaBlockNode l (LambdaBlock l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocationNode l
    -- | Invoking a specific named method.
    = MethodCallNode l (MethodCall l)
    -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    | PrimaryMethodCallNode l (PrimaryMethodCall l)
    -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    | SuperMethodCallNode l (SuperMethodCall l)
    -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    | ClassMethodCallNode l (ClassMethodCall l)
    -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    | TypeMethodCallNode l (TypeMethodCall l)
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-----------------------------------------------------------------------
-- Elements

-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit l = CompilationUnit
    { infoCompUnit    :: l
    , packageLocation :: Maybe (PackageDecl l)
    , imports         :: [ImportDecl l]
    , typeDecls       :: [TypeDeclNode l]
    }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

data ModuleDeclaration l = ModuleDeclaration
    { infoModuleDecl :: l
    , modulePackage  :: Package
    , moduleSpecs    :: [ModuleSpecNode l]
    }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
data PackageDecl l = PackageDecl { infoPackDec :: l, packageDecl :: Package}
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | requires the module to work
data ModuleRequires l = ModuleRequires
  { infoModuleRequires :: l
  , requireModule      :: Package
  }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

  -- | exports the package
data ModuleExports l = ModuleExports
  { infoModuleExports :: l
  , exportsPackage    :: Package
  }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl l = ImportDecl
  { infoImportDecl :: l
  , staticImport   :: Bool
  , importPackage  :: Package
  }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-----------------------------------------------------------------------
-- Declarations


-- | A type declaration declares a class type or an interface type.
data ClassTypeDecl l = ClassTypeDecl { infoClassTypeDecl :: l, classDecl :: ClassDecl l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
data InterfaceTypeDecl l = InterfaceTypeDecl { infoInterfaceTypeDecl :: l, interfaceDecl :: InterfaceDecl l}
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A class declaration specifies a new named reference type.
data ClassDecl l = ClassDecl
      { infoClassDecl      :: l
      , classDeclModifiers :: [Modifier l]
      , classDeclName      :: Ident
      , classTypeParams    :: [TypeParam]
      , extends            :: Maybe (Extends l)
      , classImplements    :: [Implements l]
      , classBody          :: ClassBody l
      }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
data EnumDecl l = EnumDecl
      { infoEnumDecl      :: l
      , enumDeclModifiers :: [Modifier l]
      , enumeDeclName     :: Ident
      , enumImplements    :: [Implements l]
      , enumBody          :: EnumBody l
      }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An extends clause
data Extends l = Extends { infoExtends :: l, extendsClass :: RefType }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An implements clause
data Implements l = Implements { infoImplements :: l, implementsInterface :: RefType }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
data ClassBody l = ClassBody { infoClassBody :: l, classDecls :: [Decl l] }
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | The body of an enum type may contain enum constants.
data EnumBody l = EnumBody { infoEnumBody :: l, enumConstans :: [EnumConstant l], enumDecls :: [Decl l] }
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | An enum constant defines an instance of the enum type.
data EnumConstant l = EnumConstant
  { infoEnumConstant :: l
  , enumConstantName :: Ident
  , enumArguments    :: [Argument l]
  , enumConstantBody :: Maybe (ClassBody l)
  }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl l = InterfaceDecl
  { infoInterfaceDecl      :: l
  , interfaceKind          :: InterfaceKind
  , interfaceDeclModifiers :: [Modifier l]
  , interfaceDeclName      :: Ident
  , interfaceTypeParams    :: [TypeParam]
  , interfaceExtends       :: [Extends l]
  , interfaceBody          :: InterfaceBody l
  }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Interface can declare either a normal interface or an annotation
data InterfaceKind = InterfaceNormal | InterfaceAnnotation
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The body of an interface may declare members of the interface.
data InterfaceBody l = InterfaceBody { infoInterfaceBody ::l, members :: [MemberDeclNode l]}
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl l = MemberDecl { infoMemberDecl :: l, member :: MemberDeclNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
data InitDecl l = InitDecl { infoInitDecl :: l, staticDecl :: Bool, statements :: Block l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | The variables of a class type are introduced by field declarations.
--
-- Example:
--
-- >>> parseCompilationUnit "public class MyClass {private String foo = \"Hello World\"; }"
-- ...
-- __FieldDecl__ {__infoFieldDecl__ = Segment (Position 1 31) (Position 1 31), __memberDeclModifiers__ = [private], __fieldType__ =
-- RefType (ClassRefType (WithoutPackage (ClassName [(Ident "String",[])]))), __fieldVarDecls__ = [VarDecl {infoVarDecl =
-- Segment (Position 1 38) (Position 1 38), varDeclName = VarId {infoVarId = Segment (Position 1 38) (Position 1 38),
-- varIdName = Ident "foo"}, varInit = Just (InitExp {infoInitExp= Segment (Position 1 44) (Position 1 44), init = Lit
-- {infoLit = Segment (Position 1 44) (Position 1 44), literal = String "Hello World"}})}]}}]}}}]})
data FieldDecl l = FieldDecl
      { infoFieldDecl       :: l
      , memberDeclModifiers :: [Modifier l]
      , fieldType           :: Type
      , fieldVarDecls       :: [VarDecl l]
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
-- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
-- Example:
--
-- >>> parseCompilationUnit "public class MyClass {private String foo() {}}"
-- ...
-- [MemberDecl {infoMemberDecl = Segment (Position 1 23) (Position 1 23), member = MethodDecl {infoMethodDecl =
-- Segment (Position 1 31) (Position 1 31), methodDeclModifiers = [private], methodTypeParams = [], returnType =
-- Just (RefType (ClassRefType (WithoutPackage (ClassName [(Ident "String",[])])))), methodDeclName = Ident "foo", params = [],
-- exceptions = [], defaultInterfaceAnnotation = Nothing, methodBody = MethodBody {infoMethodBody = Segment (Position 1 44)
-- (Position 1 44), impl= Just (Block {infoBlock = Segment (Position 1 45) (Position 1 45), blockStatements = []})}}}]}}}]})
data MethodDecl l =  MethodDecl
      { infoMethodDecl             :: l
      , methodDeclModifiers        :: [Modifier l]
      , methodTypeParams           :: [TypeParam]
      , returnType                 :: Maybe Type
      , methodDeclName             :: Ident
      , params                     :: [FormalParam l]
      , exceptions                 :: [ExceptionType l]
      , defaultInterfaceAnnotation :: Maybe (ExpNode l)
      , methodBody                 :: MethodBody l
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A constructor is used in the creation of an object that is an instance of a class.
data ConstructorDecl l = ConstructorDecl
      { infoConstructorDecl     :: l
      , constructorMod          :: [Modifier l]
      , constructorTypeParams   :: [TypeParam]
      , constructorClassName    :: Ident
      , constructorFormalParams :: [FormalParam l]
      , constructorExceptions   :: [ExceptionType l]
      , constructorBody         :: ConstructorBody l
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
-- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
data MemberClassDecl l = MemberClassDecl
      { infoMemberClassDecl :: l
      , memberClassDecl     :: ClassDecl l
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
data MemberInterfaceDecl l =  MemberInterfaceDecl
      { infoMemberInterfaceDecl :: l
      , memberInterfaceDecls    :: InterfaceDecl l
      }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl l = VarDecl { infoVarDecl :: l, varDeclName :: VarDeclIdNode l, varInit :: Maybe (VarInitNode l) }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The name of a variable in a declaration, which may be an array.
data VarId l = VarId { infoVarId :: l, varIdName :: Ident }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
-- | Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
data VarDeclArray l = VarDeclArray { infoVarDeclArray :: l, varIdDecl :: VarDeclIdNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Explicit initializer for a variable declaration.
data InitExp l = InitExp { infoInitExp :: l, init :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
data InitArray l =  InitArray { infoInitArray :: l, varArrayInit :: ArrayInit l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam l = FormalParam
  { infoFormalParam      :: l
  , formalParamModifiers :: [Modifier l]
  , paramType            :: Type
  , variableArity        :: Bool
  , paramName            :: VarDeclIdNode l
  }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Gets type of FormalParam
instance HasType (FormalParam l) where
  getType (FormalParam _ _ t _ _) =  t

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
data MethodBody l = MethodBody { infoMethodBody :: l, impl :: Maybe (Block l) }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody l = ConstructorBody
  { infoConstructorBody :: l
  , constructorInvoc    :: Maybe (ExplConstrInvNode l)
  , constrBody          :: [BlockStmt l]
  }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ThisInvoke l = ThisInvoke
      { infoThisInvoke      :: l
      , thisTypeArguments   :: [RefType]
      , thisConstrArguments :: [Argument l]
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
data SuperInvoke l = SuperInvoke
      { infoSuperInvoke      :: l
      , superTypeArguments   :: [RefType]
      , superConstrArguments :: [Argument l]
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
data PrimarySuperInvoke l = PrimarySuperInvoke
      { infoPrimarySuperInvoke :: l
      , primary                :: ExpNode l
      , primaryTypeArguments   :: [RefType]
      , primaryConstrArguments :: [Argument l]
      }
  deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data Modifier l
    = Public l
    | Private l
    | Protected l
    | Abstract l
    | Final l
    | Static l
    | StrictFP l
    | Transient l
    | Volatile l
    | Native l
    | Annotation l (Annotation l)
    | Synchronized_ l
    | DefaultModifier l
  deriving (Eq,Read,Typeable,Generic,Data)

instance (Show l) => Show (Modifier l) where
   show (Public _) = "public"
   show (Private _) = "private"
   show (Protected _) = "protected"
   show (Abstract _) = "abstract"
   show (Final _) = "final"
   show (Static _) = "static"
   show (StrictFP _) = "strictfp"
   show (Transient _) = "transient"
   show (Volatile _) = "volatile"
   show (Native _) = "native"
   show (Annotation _ a) = show a
   show (Synchronized_ _) = "synchronized"
   show (DefaultModifier _) = "default"

-- | Annotations have three different forms: no-parameter, single-parameter or key-value pairs
data Annotation l = NormalAnnotation      { annName :: Name -- Not type because not type generics not allowed
                                          , annKV   :: [(Ident, ElementValue l)] }
                | SingleElementAnnotation { annName  :: Name
                                          , annValue:: ElementValue l }
                | MarkerAnnotation        { annName :: Name }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Annotations may contain  annotations or (loosely) expressions
data ElementValue l = EVVal { infoEVVal :: l, elementVarInit :: VarInitNode l }
                  | EVAnn { infoEVAnn :: l, annotation :: Annotation l}
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
data Block l = Block { infoBlock :: l, blockStatements :: [BlockStmt l] }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt l = BlockStmt { infoBlockStmt :: l, statement :: StmtNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
data LocalClass l =  LocalClass { infoLocalClass :: l, blockLocalClassDecl :: ClassDecl l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
data LocalVars l = LocalVars
      { infoLocalVars    :: l
      , locaVarModifiers :: [Modifier l]
      , blockVarType     :: Type
      , localVarDecls    :: [VarDecl l]
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)


-- | A statement can be a nested block.
data StmtBlock l = StmtBlock { infoStmtBlock :: l, block :: Block l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | The @if-then@ statement allows conditional execution of a statement.
data IfThenElse l =  IfThenElse { infoIfThenElse :: l, ifExp :: ExpNode l, thenExp :: StmtNode l, elseExp :: Maybe (StmtNode l) }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
data While l = While { infoWhile :: l, whileVondition :: ExpNode l, whileBody :: StmtNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
data BasicFor l = BasicFor
      { infoBasicFor :: l
      , forInit      :: Maybe (ForInitNode l)
      , forCond      :: Maybe (ExpNode l)
      , forUpdate    :: Maybe [ExpNode l]
      , basicForBody :: StmtNode l
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
data EnhancedFor l = EnhancedFor
      { infoEnhancedFor  :: l
      , loopVarModifiers :: [Modifier l] -- ^ example: for (final Int x : set) {..}
      , loopVarType      :: Type
      , loopVarName      :: Ident
      , iterable         :: ExpNode l
      , enhancedForBody  :: StmtNode l
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | An empty statement does nothing.
newtype Empty l = Empty { infoEmpty :: l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
data ExpStmt l = ExpStmt { infoExpStmt :: l, exp :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
data Assert l = Assert { infoAssert :: l, booleanExp :: ExpNode l, valueExp :: Maybe (ExpNode l) }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | The switch statement transfers control to one of several statements depending on the value of an expression.
data Switch l = Switch { infoSwitch :: l, switchValue :: ExpNode l, switchBlocks :: [SwitchBlock l] }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
data Do l = Do { infoDo :: l, doBody :: StmtNode l, doCondition :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A @break@ statement transfers control out of an enclosing statement.
data Break l = Break { infoBreak :: l, breakLabel :: Maybe Ident }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
data Continue l = Continue { infoContinue :: l, continueLabel :: Maybe Ident }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A @return@ statement returns control to the invoker of a method or constructor.
data Return l = Return { infoReturn :: l, returnExp :: Maybe (ExpNode l) }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
data Synchronized l = Synchronized { infoSynchronized :: l, synchronizeOn :: ExpNode l, synchronizeBloc :: Block l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A @throw@ statement causes an exception to be thrown.
data Throw l = Throw { infoThrow :: l, throwExp :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
data Try l = Try
      { infoTry     :: l
      , tryResource :: [TryResourceNode l]
      , tryBlock    :: Block l
      , catches     :: [Catch l]
      , finally     ::  Maybe (Block l)
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)

    -- | Statements may have label prefixes.
data Labeled l = Labeled { infoLabeled :: l, label :: Ident, labeledStmt :: StmtNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch l = Catch { infoCatch :: l, catchParam :: FormalParam l, catchBlock :: Block l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

    -- | Newly declared variables
data TryResourceVar l = TryResourceVar
    { infoTryResourceVar :: l
    , resourceModifiers  :: [Modifier l]
    , resourceVarType    :: RefType -- restricted to ClassType or TypeVariable
    , resourceVarDecl    :: [VarDecl l]
    }
    deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Effectively final variable
data TryResourceFinalVar l = TryResourceFinalVar
    { infoTryResourceFinalVar :: l
    , resourceFinalVarName    :: Ident
    }
    deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock l = SwitchBlock { infoSwitchBlock :: l, switchLabel :: SwitchLabelNode l, switchStmts :: [BlockStmt l] }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

    -- | The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
data SwitchCase l = SwitchCase { infoSwitchCase :: l, switchExp :: ExpNode l}
      deriving (Eq,Show,Read,Typeable,Generic,Data)
newtype SwitchDefault l = SwitchDefault { infoDefault :: l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Initialization code for a basic @for@ statement.
data ForLocalVars l = ForLocalVars
      { infoForLocalVars :: l
      , forVarModifiers  :: [Modifier l]
      , forVarType       :: Type
      , forVarDecls      :: [VarDecl l]
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
data ForInitExps l = ForInitExps { infoForInitExps :: l, initExpr :: [ExpNode l] }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An exception type has to be a class type or a type variable.
data ExceptionType l = ExceptionType { infoExceptionType :: l, expectionType :: RefType } -- restricted to ClassType or TypeVariable
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Gets type of ExceptionType
instance HasType (ExceptionType l) where
  getType (ExceptionType _ x) = RefType x

-- | Arguments to methods and constructors are expressions.
type Argument = ExpNode

    -- | A literal denotes a fixed, unchanging value.
data Lit l = Lit { infoLit :: l, literal :: Literal }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
data ClassLit l = ClassLit { infoClassLit :: l, classLit :: Maybe Type }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
newtype This l = This { infoThis :: l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    -- TODO: Fix Parser here
data QualifiedThis l = QualifiedThis { infoQualifiedThis :: l, qualiType :: Type }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
data InstanceCreation l = InstanceCreation
      { infoInstanceCreation :: l
      , instanceTypeArgs     :: [TypeArgument]
      , instanceTypeDecl     :: TypeDeclSpecifier
      , instanceArguments    :: [Argument l]
      , anonymousClass       :: Maybe (ClassBody l)
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    {- TODO what is is the mysteryExp used for?-}
data QualInstanceCreation l = QualInstanceCreation
      { infoQualInstanceCreation    :: l
      , mysteryExp                  :: ExpNode l
      , qualiInstancetypeArgs       :: [TypeArgument]
      , className                   :: Ident
      , qualiInstanceArguments      :: [Argument l]
      , qualiInstanceAnonymousClass :: Maybe (ClassBody l)
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
data ArrayCreate l = ArrayCreate
  { infoArrayCreate :: l
  , arrayType       :: Type
  , arrayDimExprs   :: [ExpNode l]
  , dimensions      :: Int
  }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
data ArrayCreateInit l = ArrayCreateInit
  { infoArrayCreateInit :: l
  , arrayInitType       :: Type
  , arrayInitDimensions :: Int
  , arrayCreatInit      :: ArrayInit l
  }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A field access expression.
data FieldAccess l = FieldAccess { infoFieldAccess :: l, fieldAccess :: FieldAccess l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A method invocation expression.
data MethodInv l = MethodInv { infoMethodInv :: l, methodInvoc :: MethodInvocationNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | An array access expression refers to a variable that is a component of an array.
data ArrayAccess l = ArrayAccess { infoArrayAccess :: l, arrayAccessIndex :: ArrayIndex l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
{-    | ArrayAccess ExpNode ExpNode -- Should this be made into a datatype, for consistency and use with Lhs? -}
    -- | An expression name, e.g. a variable.
data ExpName l = ExpName { infoExpName :: l, expName :: Name }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Post-incrementation expression, i.e. an expression followed by @++@.
data PostIncrement l = PostIncrement { infoPostIncrement :: l, postIncExp :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Post-decrementation expression, i.e. an expression followed by @--@.
data PostDecrement l = PostDecrement { infoPostDecrement :: l, postDecExp :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
data PreIncrement l = PreIncrement { infoPreIncrement :: l, preIncExp :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
data PreDecrement l = PreDecrement { infoPreDecrement :: l, preDecExp :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
data PrePlus l = PrePlus  { infoPrePlus :: l, plusArg :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
data PreMinus l = PreMinus { infoPreMinus :: l, minusArg :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
data PreBitCompl l = PreBitCompl { infoPreBitCompl :: l, bitComplArg :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Logical complementation of boolean values.
data PreNot l = PreNot { infoPreNot :: l, notArg :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
data Cast l = Cast { infoCast :: l, castTarget :: Type, castArg :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | The application of a binary operator to two operand expressions.
data BinOp l = BinOp { infoBinOp :: l, binArgLeft :: ExpNode l, binOp :: Op, binOpRight :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Testing whether the result of an expression is an instance of some reference type.
data InstanceOf l = InstanceOf { infoInstanceOf :: l, instanceOfArg :: ExpNode l, instanceOfTarget :: RefType }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
data Cond l = Cond { infoCond :: l, condition :: ExpNode l, conditionTrueExp :: ExpNode l, conditionFalseExp :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Assignment of the result of an expression to a variable.
data Assign l = Assign { infoAssign :: l, assignTarget :: LhsNode l, assignOp :: AssignOp, assignSource :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Lambda expression
data Lambda l = Lambda { infoLambda :: l, lambdaParams :: LambdaParamsNode l, lambdaExpression :: LambdaExpression l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Method reference
data MethodRef l = MethodRef { infoMethodRef :: l, methodClass :: Name, methodName :: Ident }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Assign to a variable
data NameLhs l = NameLhs { infoNameLhs :: l, varLhsName :: Name }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
-- | Assign through a field access
data FieldLhs l = FieldLhs { infoFieldLhs :: l, fieldLhsName :: FieldAccess l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
-- | Assign to an array
data ArrayLhs l = ArrayLhs { infoArrayLhs :: l, arrayLhsIndex :: ArrayIndex l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Array access
data ArrayIndex l = ArrayIndex
  { infoArrayIndex :: l
  , arrayName      :: ExpNode l
  , arrayIndices   :: [ExpNode l]
  }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Accessing a field of an object or array computed from an expression.
data PrimaryFieldAccess l = PrimaryFieldAccess
  { infoPrimaryFieldAccess :: l
  , targetObject           :: ExpNode l
  , targetField            :: Ident
  }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
-- | Accessing a field of the superclass.
data SuperFieldAccess l = SuperFieldAccess { infoSuperFieldAccess :: l, superField :: Ident }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
-- | Accessing a (static) field of a named class.
data ClassFieldAccess l = ClassFieldAccess { infoClassFieldAccess :: l, targetClass :: Name, staticField :: Ident }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaSingleParam l = LambdaSingleParam { infoLambdaSingleParam :: l, lambdaParamName :: Ident }
    deriving (Eq,Show,Read,Typeable,Generic,Data)
data LambdaFormalParams l = LambdaFormalParams { infoLambdaFormalParams :: l, lambdaFormalParams :: [FormalParam l] }
    deriving (Eq,Show,Read,Typeable,Generic,Data)
data LambdaInferredParams l = LambdaInferredParams { infoLambdaInferredParams :: l, lambdaParamNames :: [Ident] }
    deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Lambda expression, starting from java 8
data LambdaExpression l = LambdaExpression { infoLambdaExpression ::l, singleLambdaExp :: ExpNode l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
-- | Lambda block, starting from java 8
data LambdaBlock l = LambdaBlock { infoLambdaBlock :: l, lambdaBlock :: Block l }
  deriving (Eq,Show,Read,Typeable,Generic,Data)


    -- | Invoking a specific named method.
data MethodCall l = MethodCall { infoMethodCall :: l, methodCallName :: Name, methodCallArgs :: [Argument l] }
  deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
data PrimaryMethodCall l = PrimaryMethodCall
      { infoPrimaryMethodCall :: l
      , methodCallTargetObj   :: ExpNode l
      , mysteryRefTypes       :: [RefType] {- TODO: mysteryRefTypes, prob. type args. not set in Parser -}
      , primaryMethodName     :: Ident
      , primaryMethodCallArgs :: [Argument l]
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Invoking a method of the super class, giving arguments for any generic type parameters.
data SuperMethodCall l = SuperMethodCall
      { infoSuperMethodCall :: l
      , superMethodTypeArgs :: [RefType]
      , superMethodName     :: Ident
      , superMethodArgs     :: [Argument l]
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
data ClassMethodCall l = ClassMethodCall
      { infoClassMethodCall :: l
      , methodClassTarget   :: Name
      , classMethodTypeArgs :: [RefType]
      , classMethodName     :: Ident
      , classMethodArgs     :: [Argument l]
      }
      deriving (Eq,Show,Read,Typeable,Generic,Data)
    -- | Invoking a method of a named type, giving arguments for any generic type parameters.
data TypeMethodCall l = TypeMethodCall
      { infoTypeMethodCall    :: l
      , typeMethodClassTarget :: Name
      , typeMethodTypeArgs    :: [RefType]
      , typeMethodName        :: Ident
      , typeMethodArgs        :: [Argument l]
      }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
data ArrayInit l = ArrayInit { infoArrayInit :: l, arrayInits :: [VarInitNode l] }
  deriving (Eq,Show,Read,Typeable,Generic,Data)

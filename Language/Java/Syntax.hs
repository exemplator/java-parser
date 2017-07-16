{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.Java.Syntax
    ( CompilationUnit(..)
    , PackageDecl(..)
    , ImportDecl(..)
    , TypeDecl(..)
    , ClassDecl(..)
    , ClassBody(..)
    , EnumBody(..)
    , EnumConstant(..)
    , InterfaceDecl(..)
    , InterfaceBody(..)
    , InterfaceKind(..)
    , Decl(..)
    , MemberDecl(..)
    , VarDecl(..)
    , VarDeclId(..)
    , VarInit(..)
    , FormalParam(..)
    , MethodBody(..)
    , ConstructorBody(..)
    , ExplConstrInv(..)
    , Modifier(..)
    , Annotation(..)
    , desugarAnnotation
    , desugarAnnotation'
    , ElementValue(..)
    , Block(..)
    , BlockStmt(..)
    , Stmt(..)
    , Catch(..)
    , SwitchBlock(..)
    , SwitchLabel(..)
    , ForInit(..)
    , ExceptionType(..)
    , Argument
    , Exp(..)
    , Lhs(..)
    , ArrayIndex(..)
    , FieldAccess(..)
    , LambdaParams(..)
    , LambdaExpression(..)
    , ArrayInit(..)
    , MethodInvocation(..)
    , module Language.Java.Syntax.Exp
    , module Language.Java.Syntax.Types
    ) where

import           Data.Data
import           Data.Maybe
import           GHC.Generics               (Generic)

import           Language.Java.Position
import           Language.Java.Syntax.Exp
import           Language.Java.Syntax.Types

-----------------------------------------------------------------------
-- Packages


-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit = CompilationUnit (Maybe PackageDecl) [ImportDecl] [TypeDecl]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
data PackageDecl = PackageDecl Segment Package
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl
    = ImportDecl Segment Bool {- static? -} Name Bool {- .*? -}
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-----------------------------------------------------------------------
-- Declarations


-- | A type declaration declares a class type or an interface type.
data TypeDecl
    = ClassTypeDecl ClassDecl
    | InterfaceTypeDecl InterfaceDecl
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment TypeDecl where
  getSegment (ClassTypeDecl cd) = getSegment cd
  getSegment (InterfaceTypeDecl ifd) = getSegment ifd

instance HasType TypeDecl where
  getType (ClassTypeDecl cd) = getType cd
  getType (InterfaceTypeDecl ifd) = getType ifd

-- | A class declaration specifies a new named reference type.
data ClassDecl
    = ClassDecl Segment [Modifier] Ident [TypeParam] (Maybe RefType) [RefType] ClassBody
    | EnumDecl Segment [Modifier] Ident [RefType] EnumBody
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment ClassDecl where
  getSegment (ClassDecl s _ _ _ _ _ _) = s
  getSegment (EnumDecl s _ _ _ _) = s

-- | Get type of ClassDecl
instance HasType ClassDecl where
  getType (ClassDecl _ _ i _ _ _ _) =  withoutPackageIdentToType i
  getType (EnumDecl _ _ i _ _) =  withoutPackageIdentToType i

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
data ClassBody = ClassBody Segment [Decl]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment ClassBody where
  getSegment (ClassBody s _ ) = s

-- | The body of an enum type may contain enum constants.
data EnumBody = EnumBody Segment [EnumConstant] [Decl]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment EnumBody where
  getSegment (EnumBody s _ _ ) = s

-- | An enum constant defines an instance of the enum type.
data EnumConstant = EnumConstant Segment Ident [Argument] (Maybe ClassBody)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment EnumConstant where
  getSegment (EnumConstant s _ _ _) = s

-- | Get type of EnumConstant
instance HasType EnumConstant where
  getType (EnumConstant _ i _ _) = withoutPackageIdentToType i

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl
    = InterfaceDecl Segment InterfaceKind [Modifier] Ident [TypeParam] [RefType] InterfaceBody
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment InterfaceDecl where
  getSegment (InterfaceDecl s _ _ _ _ _ _) = s

-- | Get type of InterfaceDecl
instance HasType InterfaceDecl where
  getType (InterfaceDecl _ _ _ i _ _ _) = withoutPackageIdentToType i

-- | Interface can declare either a normal interface or an annotation
data InterfaceKind = InterfaceNormal | InterfaceAnnotation
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The body of an interface may declare members of the interface.
data InterfaceBody
    = InterfaceBody Segment [MemberDecl]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment InterfaceBody where
  getSegment (InterfaceBody s _) = s

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl
    = MemberDecl Segment MemberDecl
    | InitDecl Segment Bool Block
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment Decl where
  getSegment (MemberDecl s _ ) = s
  getSegment (InitDecl s _ _) = s

-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl
    -- | The variables of a class type are introduced by field declarations.
    = FieldDecl Segment [Modifier] Type [VarDecl]
    -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    | MethodDecl Segment [Modifier] [TypeParam] (Maybe Type) Ident [FormalParam] [ExceptionType] (Maybe Exp) MethodBody
    -- | A constructor is used in the creation of an object that is an instance of a class.
    | ConstructorDecl Segment [Modifier] [TypeParam] Ident [FormalParam] [ExceptionType] ConstructorBody
    -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    | MemberClassDecl Segment ClassDecl
    -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    | MemberInterfaceDecl Segment InterfaceDecl
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment MemberDecl where
  getSegment (FieldDecl s _ _ _) = s
  getSegment (MethodDecl s _ _ _ _ _ _ _ _) = s
  getSegment (ConstructorDecl s _ _ _ _ _ _) = s
  getSegment (MemberClassDecl s _ ) = s
  getSegment (MemberInterfaceDecl s _ ) = s

-- | Get type of MemberDecl if it is a MethodDecl (our solution to handeling the Maybe)
instance CollectTypes MemberDecl where
  collectTypes (FieldDecl _ _ t _) =  [t]
  collectTypes (MethodDecl _ _ _ t _ _ _ _ _) =  maybeToList t
  collectTypes ConstructorDecl{} = []
  collectTypes (MemberClassDecl _ cd) =  [getType cd]
  collectTypes (MemberInterfaceDecl _ idecl) =  [getType idecl]

-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl
    = VarDecl Segment VarDeclId (Maybe VarInit)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment VarDecl where
  getSegment (VarDecl s _ _ ) = s

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId
    = VarId Segment Ident
    | VarDeclArray Segment VarDeclId
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment VarDeclId where
  getSegment (VarId s _ ) = s
  getSegment (VarDeclArray s _ ) = s

-- | Explicit initializer for a variable declaration.
data VarInit
    = InitExp Segment Exp
    | InitArray Segment ArrayInit
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment VarInit where
  getSegment (InitExp s _ ) = s
  getSegment (InitArray s _ ) = s

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam = FormalParam Segment [Modifier] Type Bool VarDeclId
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment FormalParam where
  getSegment (FormalParam s _ _ _ _) = s

-- | Gets type of FormalParam
instance HasType FormalParam where
  getType (FormalParam _ _ t _ _) =  t

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
data MethodBody = MethodBody Segment (Maybe Block)
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment MethodBody where
  getSegment (MethodBody s _) = s

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody = ConstructorBody Segment (Maybe ExplConstrInv) [BlockStmt]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment ConstructorBody where
  getSegment (ConstructorBody s _ _) = s

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv
    = ThisInvoke             Segment [RefType] [Argument]
    | SuperInvoke            Segment [RefType] [Argument]
    | PrimarySuperInvoke     Segment Exp [RefType] [Argument]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment ExplConstrInv where
  getSegment (ThisInvoke s _ _) = s
  getSegment (SuperInvoke s _ _) = s
  getSegment (PrimarySuperInvoke s _ _ _) = s


-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data Modifier
    = Public
    | Private
    | Protected
    | Abstract
    | Final
    | Static
    | StrictFP
    | Transient
    | Volatile
    | Native
    | Annotation Annotation
    | Synchronized_
  deriving (Eq,Read,Typeable,Generic,Data)

instance Show Modifier where
   show Public = "public"
   show Private = "private"
   show Protected = "protected"
   show Abstract = "abstract"
   show Final = "final"
   show Static = "static"
   show StrictFP = "strictfp"
   show Transient = "transient"
   show Volatile = "volatile"
   show Native = "native"
   show (Annotation a) = show a
   show Synchronized_ = "synchronized"

-- | Annotations have three different forms: no-parameter, single-parameter or key-value pairs
data Annotation = NormalAnnotation        { annName :: Name -- Not type because not type generics not allowed
                                          , annKV   :: [(Ident, ElementValue)] }
                | SingleElementAnnotation { annName  :: Name
                                          , annValue:: ElementValue }
                | MarkerAnnotation        { annName :: Name }
  deriving (Eq,Show,Read,Typeable,Generic,Data)


desugarAnnotation :: Annotation -> (Name, [(Ident, ElementValue)])
desugarAnnotation (MarkerAnnotation n)          = (n, [])
desugarAnnotation (SingleElementAnnotation n e) = (n, [(Ident "value", e)])
desugarAnnotation (NormalAnnotation n kv)       = (n, kv)
desugarAnnotation' :: Annotation -> Annotation
desugarAnnotation' = uncurry NormalAnnotation . desugarAnnotation

-- | Annotations may contain  annotations or (loosely) expressions
data ElementValue = EVVal Segment VarInit
                  | EVAnn Segment Annotation
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment ElementValue where
  getSegment (EVVal s _) = s
  getSegment (EVVal s _) = s

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
data Block = Block Segment [BlockStmt]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment Block where
  getSegment (Block s _) = s

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt
    = BlockStmt Segment Stmt
    | LocalClass Segment ClassDecl
    | LocalVars Segment [Modifier] Type [VarDecl]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment BlockStmt where
  getSegment (BlockStmt s _ ) = s
  getSegment (LocalClass s _ ) = s
  getSegment (LocalVars s _ _ _) = s

-- | A Java statement.
data Stmt
    -- | A statement can be a nested block.
    = StmtBlock Segment Block
    -- | The @if-then@ statement allows conditional execution of a statement.
    | IfThen Exp Segment Stmt
    -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    | IfThenElse Segment Exp Stmt Stmt
    -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    | While Exp Segment Stmt
    -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    | BasicFor Segment (Maybe ForInit) (Maybe Exp) (Maybe [Exp]) Stmt
    -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    | EnhancedFor Segment [Modifier] Type Ident Exp Stmt
    -- | An empty statement does nothing.
    | Empty
    -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    | ExpStmt Segment Exp
    -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    | Assert Segment Exp (Maybe Exp)
    -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    | Switch Segment Exp [SwitchBlock]
    -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    | Do Stmt Segment Exp
    -- | A @break@ statement transfers control out of an enclosing statement.
    | Break Segment (Maybe Ident)
    -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    | Continue Segment (Maybe Ident)
    -- A @return@ statement returns control to the invoker of a method or constructor.
    | Return Segment (Maybe Exp)
    -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    | Synchronized Segment Exp Block
    -- | A @throw@ statement causes an exception to be thrown.
    | Throw Segment Exp
    -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
    | Try Block Segment [Catch] (Maybe {- finally -} Block)
    -- | Statements may have label prefixes.
    | Labeled Segment Ident Stmt
  deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment BlockStmt where
  getSegment (BlockStmt s _ ) = s
  getSegment (LocalClass s _ ) = s
  getSegment (LocalVars s _ _ _) = s

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch = Catch Segment FormalParam Block
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock
    = SwitchBlock Segment SwitchLabel [BlockStmt]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A label within a @switch@ statement.
data SwitchLabel
    -- | The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
    = SwitchCase Segment Exp
    | Default Segment
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Initialization code for a basic @for@ statement.
data ForInit
    = ForLocalVars Segment [Modifier] Type [VarDecl]
    | ForInitExps Segment [Exp]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An exception type has to be a class type or a type variable.
data ExceptionType = ExceptionType Segment RefType -- restricted to ClassType or TypeVariable
  deriving (Eq,Show,Read,Typeable,Generic,Data) --TODO segment

deriving instance HasSegment ExceptionType

-- | Arguments to methods and constructors are expressions.
type Argument = Exp

-- | A Java expression.
data Exp
    -- | A literal denotes a fixed, unchanging value.
    = Lit Segment Literal
    -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    | ClassLit Segment (Maybe Type)
    -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    | This Segment
    -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    -- TODO: Fix Parser here
    | QualifiedThis Segment Type
    -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    | InstanceCreation Segment [TypeArgument] TypeDeclSpecifier [Argument] (Maybe ClassBody)
    -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    | QualInstanceCreation Segment Exp [TypeArgument] Ident [Argument] (Maybe ClassBody)
    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    | ArrayCreate Segment Type [Exp] Int
    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    | ArrayCreateInit Segment Type Int ArrayInit
    -- | A field access expression.
    | FieldAccess Segment FieldAccess
    -- | A method invocation expression.
    | MethodInv Segment MethodInvocation
    -- | An array access expression refers to a variable that is a component of an array.
    | ArrayAccess Segment ArrayIndex
{-    | ArrayAccess Exp Exp -- Should this be made into a datatype, for consistency and use with Lhs? -}
    -- | An expression name, e.g. a variable.
    | ExpName Segment Name
    -- | Post-incrementation expression, i.e. an expression followed by @++@.
    | PostIncrement Segment Exp
    -- | Post-decrementation expression, i.e. an expression followed by @--@.
    | PostDecrement Segment Exp
    -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    | PreIncrement Segment Exp
    -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    | PreDecrement Segment Exp
    -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    | PrePlus  Segment Exp
    -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    | PreMinus Segment Exp
    -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    | PreBitCompl Segment Exp
    -- | Logical complementation of boolean values.
    | PreNot Segment Exp
    -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    | Cast Type Segment Exp
    -- | The application of a binary operator to two operand expressions.
    | BinOp Segment Exp Op Exp
    -- | Testing whether the result of an expression is an instance of some reference type.
    | InstanceOf Segment Exp RefType
    -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    | Cond Segment Exp Exp Exp
    -- | Assignment of the result of an expression to a variable.
    | Assign Segment Lhs AssignOp Exp
    -- | Lambda expression
    | Lambda Segment LambdaParams LambdaExpression
    -- | Method reference
    | MethodRef Segment Name Ident
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data Lhs
    = NameLhs Segment Name          -- ^ Assign to a variable
    | FieldLhs Segment FieldAccess  -- ^ Assign through a field access
    | ArrayLhs Segment ArrayIndex   -- ^ Assign to an array
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Array access
data ArrayIndex = ArrayIndex Segment Exp [Exp]    -- ^ Index into an array
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccess
    = PrimaryFieldAccess Segment Exp Ident      -- ^ Accessing a field of an object or array computed from an expression.
    | SuperFieldAccess Segment Ident            -- ^ Accessing a field of the superclass.
    | ClassFieldAccess Segment Name Ident       -- ^ Accessing a (static) field of a named class.
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaParams
  = LambdaSingleParam Segment Ident
  | LambdaFormalParams Segment [FormalParam]
  | LambdaInferredParams Segment [Ident]
    deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Lambda expression, starting from java 8
data LambdaExpression
    = LambdaExpression Segment Exp
    | LambdaBlock Segment Block
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocation
    -- | Invoking a specific named method.
    = MethodCall Segment Name [Argument]
    -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    | PrimaryMethodCall Segment Exp [RefType] Ident [Argument]
    -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    | SuperMethodCall Segment [RefType] Ident [Argument]
    -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    | ClassMethodCall Segment Name [RefType] Ident [Argument]
    -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    | TypeMethodCall  Segment Name [RefType] Ident [Argument]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
data ArrayInit
    = ArrayInit Segment [VarInit]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

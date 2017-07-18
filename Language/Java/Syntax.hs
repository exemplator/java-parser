{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

import           Language.Java.Syntax.Exp
import           Language.Java.Syntax.Types

-----------------------------------------------------------------------
-- Packages


-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit l = CompilationUnit l (Maybe (PackageDecl l)) [ImportDecl l] [TypeDecl l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
data PackageDecl l = PackageDecl l Package
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl l
    = ImportDecl l Bool {- static? -} Package {- .*? -}
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-----------------------------------------------------------------------
-- Declarations


-- | A type declaration declares a class type or an interface type.
data TypeDecl l
    = ClassTypeDecl l (ClassDecl l)
    | InterfaceTypeDecl l (InterfaceDecl l)
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | Get type of TypeDecl
instance HasType (TypeDecl l) where
  getType (ClassTypeDecl _ ctd) = getType ctd
  getType (InterfaceTypeDecl _ itd) = getType itd

-- | A class declaration specifies a new named reference type.
data ClassDecl l
    = ClassDecl l [Modifier l] Ident [TypeParam] (Maybe RefType) [RefType] (ClassBody l)
    | EnumDecl l [Modifier l] Ident [RefType] (EnumBody l)
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | Get type of ClassDecl
instance HasType (ClassDecl l) where
  getType (ClassDecl _ _ i _ _ _ _) =  withoutPackageIdentToType i
  getType (EnumDecl _ _ i _ _) =  withoutPackageIdentToType i

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
data ClassBody l = ClassBody l [Decl l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | The body of an enum type may contain enum constants.
data EnumBody l = EnumBody l [EnumConstant l] [Decl l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | An enum constant defines an instance of the enum type.
data EnumConstant l = EnumConstant l Ident [Argument l] (Maybe (ClassBody l))
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | Get type of EnumConstant
instance HasType (EnumConstant l) where
  getType (EnumConstant _ i _ _) =  withoutPackageIdentToType i

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl l
    = InterfaceDecl l InterfaceKind [Modifier l] Ident [TypeParam] [RefType] (InterfaceBody l)
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | Get type of InterfaceDecl
instance HasType (InterfaceDecl l) where
  getType (InterfaceDecl _ _ _ i _ _ _) =  withoutPackageIdentToType i

-- | Interface can declare either a normal interface or an annotation
data InterfaceKind = InterfaceNormal | InterfaceAnnotation
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The body of an interface may declare members of the interface.
data InterfaceBody l
    = InterfaceBody l [MemberDecl l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl l
    = MemberDecl l (MemberDecl l)
    | InitDecl l Bool (Block l)
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)


-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl l
    -- | The variables of a class type are introduced by field declarations.
    = FieldDecl l [Modifier l] Type [VarDecl l]
    -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    | MethodDecl l [Modifier l] [TypeParam] (Maybe Type) Ident [FormalParam l] [ExceptionType l] (Maybe (Exp l)) (MethodBody l)
    -- | A constructor is used in the creation of an object that is an instance of a class.
    | ConstructorDecl l [Modifier l] [TypeParam] Ident [FormalParam l] [ExceptionType l] (ConstructorBody l)
    -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    | MemberClassDecl l (ClassDecl l)
    -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    | MemberInterfaceDecl l (InterfaceDecl l)
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | Get type of MemberDecl if it is a MethodDecl (our solution to handeling the Maybe)
instance CollectTypes (MemberDecl l) where
  collectTypes (FieldDecl _ _ t _) =  [t]
  collectTypes (MethodDecl _ _ _ t _ _ _ _ _) =  maybeToList t
  collectTypes ConstructorDecl{} = []
  collectTypes (MemberClassDecl _ cd) =  [getType cd]
  collectTypes (MemberInterfaceDecl _ idecl) =  [getType idecl]

-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl l
    = VarDecl (VarDeclId l) (Maybe (VarInit l))
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId l
    = VarId l Ident
    | VarDeclArray l (VarDeclId l)
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | Explicit initializer for a variable declaration.
data VarInit l
    = InitExp l (Exp l)
    | InitArray l (ArrayInit l)
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam l = FormalParam l [Modifier l] Type Bool (VarDeclId l)
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | Gets type of FormalParam
instance HasType (FormalParam l) where
  getType (FormalParam _ _ t _ _) =  t

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
data MethodBody l = MethodBody l (Maybe (Block l))
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody l = ConstructorBody l (Maybe (ExplConstrInv l)) [BlockStmt l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv l
    = ThisInvoke l               [RefType] [Argument l]
    | SuperInvoke l              [RefType] [Argument l]
    | PrimarySuperInvoke l (Exp l) [RefType] [Argument l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)


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
  deriving (Eq,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

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

-- | Annotations have three different forms: no-parameter, single-parameter or key-value pairs
data Annotation l = NormalAnnotation      { annName :: Name -- Not type because not type generics not allowed
                                          , annKV   :: [(Ident, ElementValue l)] }
                | SingleElementAnnotation { annName  :: Name
                                          , annValue:: ElementValue l }
                | MarkerAnnotation        { annName :: Name }
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

desugarAnnotation :: Annotation l -> (Name, [(Ident, ElementValue l)])
desugarAnnotation (MarkerAnnotation n)          = (n, [])
desugarAnnotation (SingleElementAnnotation n e) = (n, [(Ident "value", e)])
desugarAnnotation (NormalAnnotation n kv)       = (n, kv)
desugarAnnotation' :: Annotation l -> Annotation l
desugarAnnotation' = uncurry NormalAnnotation . desugarAnnotation

-- | Annotations may contain  annotations or (loosely) expressions
data ElementValue l = EVVal l (VarInit l)
                  | EVAnn l (Annotation l)
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
data Block l = Block l [BlockStmt l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt l
    = BlockStmt l (Stmt l)
    | LocalClass l (ClassDecl l)
    | LocalVars l [Modifier l] Type [VarDecl l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)


-- | A Java statement.
data Stmt l
    -- | A statement can be a nested block.
    = StmtBlock l (Block l)
    -- | The @if-then@ statement allows conditional execution of a statement.
    | IfThen l (Exp l) (Stmt l)
    -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    | IfThenElse l (Exp l) (Stmt l) (Stmt l)
    -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    | While l (Exp l) (Stmt l)
    -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    | BasicFor l (Maybe (ForInit l)) (Maybe (Exp l)) (Maybe [Exp l]) (Stmt l)
    -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    | EnhancedFor l [Modifier l] Type Ident (Exp l) (Stmt l)
    -- | An empty statement does nothing.
    | Empty l
    -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    | ExpStmt l (Exp l)
    -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    | Assert l (Exp l) (Maybe (Exp l))
    -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    | Switch l (Exp l) [SwitchBlock l]
    -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    | Do l (Stmt l) (Exp l)
    -- | A @break@ statement transfers control out of an enclosing statement.
    | Break l (Maybe Ident)
    -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    | Continue l (Maybe Ident)
    -- A @return@ statement returns control to the invoker of a method or constructor.
    | Return l (Maybe (Exp l))
    -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    | Synchronized l (Exp l) (Block l)
    -- | A @throw@ statement causes an exception to be thrown.
    | Throw l (Exp l)
    -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
    | Try l (Block l) [Catch l] (Maybe {- finally -} (Block l))
    -- | Statements may have label prefixes.
    | Labeled l Ident (Stmt l)
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch l = Catch l (FormalParam l) (Block l)
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock l = SwitchBlock l (SwitchLabel l) [BlockStmt l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | A label within a @switch@ statement.
data SwitchLabel l
    -- | The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
    = SwitchCase l (Exp l)
    | Default l
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | Initialization code for a basic @for@ statement.
data ForInit l
    = ForLocalVars l [Modifier l] Type [VarDecl l]
    | ForInitExps l [Exp l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | An exception type has to be a class type or a type variable.
data ExceptionType l = ExceptionType l RefType -- restricted to ClassType or TypeVariable
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | Gets type of ExceptionType
instance HasType (ExceptionType l) where
  getType (ExceptionType _ x) = RefType x

-- | Arguments to methods and constructors are expressions.
type Argument = Exp

-- | A Java expression.
data Exp l
    -- | A literal denotes a fixed, unchanging value.
    = Lit l Literal
    -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    | ClassLit l (Maybe Type)
    -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    | This l
    -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    -- TODO: Fix Parser here
    | QualifiedThis l Type
    -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    | InstanceCreation l [TypeArgument] TypeDeclSpecifier [Argument l] (Maybe (ClassBody l))
    -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    | QualInstanceCreation l (Exp l) [TypeArgument] Ident [Argument l] (Maybe (ClassBody l))
    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    | ArrayCreate l Type [Exp l] Int
    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    | ArrayCreateInit l Type Int (ArrayInit l)
    -- | A field access expression.
    | FieldAccess l (FieldAccess l)
    -- | A method invocation expression.
    | MethodInv l (MethodInvocation l)
    -- | An array access expression refers to a variable that is a component of an array.
    | ArrayAccess l (ArrayIndex l)
{-    | ArrayAccess Exp Exp -- Should this be made into a datatype, for consistency and use with Lhs? -}
    -- | An expression name, e.g. a variable.
    | ExpName l Name
    -- | Post-incrementation expression, i.e. an expression followed by @++@.
    | PostIncrement l (Exp l)
    -- | Post-decrementation expression, i.e. an expression followed by @--@.
    | PostDecrement l (Exp l)
    -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    | PreIncrement l (Exp l)
    -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    | PreDecrement l (Exp l)
    -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    | PrePlus  l (Exp l)
    -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    | PreMinus l (Exp l)
    -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    | PreBitCompl l (Exp l)
    -- | Logical complementation of boolean values.
    | PreNot l (Exp l)
    -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    | Cast l Type (Exp l)
    -- | The application of a binary operator to two operand expressions.
    | BinOp l (Exp l) Op (Exp l)
    -- | Testing whether the result of an expression is an instance of some reference type.
    | InstanceOf l (Exp l) RefType
    -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    | Cond l (Exp l) (Exp l) (Exp l)
    -- | Assignment of the result of an expression to a variable.
    | Assign l (Lhs l) AssignOp (Exp l)
    -- | Lambda expression
    | Lambda l (LambdaParams l) (LambdaExpression l)
    -- | Method reference
    | MethodRef l Name Ident
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data Lhs l
    = NameLhs l Name          -- ^ Assign to a variable
    | FieldLhs l (FieldAccess l)  -- ^ Assign through a field access
    | ArrayLhs l (ArrayIndex l)   -- ^ Assign to an array
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | Array access
data ArrayIndex l = ArrayIndex l (Exp l) [Exp l]    -- ^ Index into an array
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccess l
    = PrimaryFieldAccess l (Exp l) Ident      -- ^ Accessing a field of an object or array computed from an expression.
    | SuperFieldAccess l Ident            -- ^ Accessing a field of the superclass.
    | ClassFieldAccess l Name Ident       -- ^ Accessing a (static) field of a named class.
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaParams l
  = LambdaSingleParam l Ident
  | LambdaFormalParams l [FormalParam l]
  | LambdaInferredParams l [Ident]
    deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | Lambda expression, starting from java 8
data LambdaExpression l
    = LambdaExpression l (Exp l)
    | LambdaBlock l (Block l)
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocation l
    -- | Invoking a specific named method.
    = MethodCall l Name [Argument l]
    -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    | PrimaryMethodCall l (Exp l) [RefType] Ident [Argument l]
    -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    | SuperMethodCall l [RefType] Ident [Argument l]
    -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    | ClassMethodCall l Name [RefType] Ident [Argument l]
    -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    | TypeMethodCall l Name [RefType] Ident [Argument l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
data ArrayInit l
    = ArrayInit l [VarInit l]
  deriving (Eq,Show,Read,Typeable,Generic,Data,Functor,Foldable,Traversable)

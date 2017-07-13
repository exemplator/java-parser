{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Language.Java.Syntax.Types where

import           Data.Data
import           GHC.Generics (Generic)

-- | There are two kinds of types in the Java programming language: primitive types and reference types.
data Type
    = PrimType PrimType
    | RefType RefType
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | There are three kinds of reference types: class types, interface types, and array types.
--   Reference types may be parameterized with type arguments.
--   Type variables cannot be syntactically distinguished from class type identifiers,
--   and are thus represented uniformly as single ident class types.
data RefType
    = ClassRefType ClassType
    {- | TypeVariable Ident -}
    | ArrayType Type
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A ClassType can either be with an package or without.
data ClassType
  = WithPackage Package [(Ident, [TypeArgument])]
  | WithoutPackage [(Ident, [TypeArgument])]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | represents a package, e.g. java.util
-- a package can either be fully qualified ("java.util"), or end with an wildcard ("java.util.*")
data Package = FullQualiPackage [Ident] | WildcardPackage [Ident]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Type arguments may be either reference types or wildcards.
data TypeArgument
    = Wildcard (Maybe WildcardBound)
    | ActualType RefType
  deriving (Eq,Show,Read,Typeable,Generic,Data)

data TypeDeclSpecifier
    = TypeDeclSpecifier ClassType
    | TypeDeclSpecifierWithDiamond ClassType Ident Diamond
    | TypeDeclSpecifierUnqualifiedWithDiamond Ident Diamond
  deriving (Eq,Show,Read,Typeable,Generic,Data)

data Diamond = Diamond
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
data WildcardBound
    = ExtendsBound RefType
    | SuperBound RefType
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A primitive type is predefined by the Java programming language and named by its reserved keyword.
data PrimType
    = BooleanT
    | ByteT
    | ShortT
    | IntT
    | LongT
    | CharT
    | FloatT
    | DoubleT
  deriving (Eq,Show,Read,Typeable,Generic,Data,Enum,Bounded)


-- | A class is generic if it declares one or more type variables. These type variables are known
--   as the type parameters of the class.
data TypeParam = TypeParam Ident [RefType]
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
newtype Ident = Ident String
    deriving (Eq,Ord,Show,Read,Typeable,Generic,Data)

fromIdent :: Ident -> String
fromIdent (Ident s) = s

-- | A name, i.e. a period-separated list of identifiers.
newtype Name = Name [Ident]
    deriving (Eq,Ord,Show,Read,Typeable,Generic,Data)

-----------------------------------------------------------------------
-- extensions and functionality

-- | A type with relaxed equality checking e.g. boxed primitives equal primitives
-- and of one of the
newtype RelaxedType = MakeRelaxed Type

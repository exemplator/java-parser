{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Syntax.Types where

import           Data.Data
import           GHC.Generics (Generic)

-- | There are two kinds of types in the Java programming language: primitive types and reference types.
data Type
    = PrimType PrimType
    | RefType RefType
  deriving (Eq,Show,Read,Typeable,Generic,Data)

class HasType a where
  getType :: a -> Type

instance HasType a => CollectTypes a where
  collectTypes x = [getType x]

class CollectTypes a where
  collectTypes :: a -> [Type]

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
    | Diamond
  deriving (Eq,Show,Read,Typeable,Generic,Data)

newtype TypeDeclSpecifier = TypeDeclSpecifier ClassType
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

-- | A type with relaxed equality checking:
-- 1. If packages equal
-- 2. Or if classes equal
-- 3. Or if boxed primitives equal primitives
newtype RelaxedType = RelaxedType Type

-- | Defines Equals for RelaxedType
instance Eq RelaxedType where
  RelaxedType (PrimType t1) == RelaxedType (PrimType t2) = t1 == t2
  RelaxedType (RefType r1) == RelaxedType (PrimType r2) = checkRelaxed r1 (primToRefType r2)
  RelaxedType (PrimType r1) == RelaxedType (RefType r2) = checkRelaxed (primToRefType r1) r2
  RelaxedType (RefType r1) == RelaxedType (RefType r2) = checkRelaxed r1 r2

-- | Checks a RelaxedType for equality.
-- A RelaxedType equals another RelaxedType if:
-- 1. If packages equal
-- 2. If classes equal
-- 3. If boxed primitives equal primitives
checkRelaxed :: RefType -> RefType -> Bool
checkRelaxed (ArrayType at1) (ArrayType at2) = RelaxedType at1 == RelaxedType at2
checkRelaxed (ArrayType _) (ClassRefType _) = False
checkRelaxed (ClassRefType _) (ArrayType _) = False
checkRelaxed (ClassRefType cr1) (ClassRefType cr2) = checkClassType cr1 cr2
  where
    checkClassType :: ClassType -> ClassType -> Bool
    checkClassType (WithPackage pack1 class1) (WithPackage pack2 class2) = pack1 == pack2 && class1 == class2
    checkClassType (WithPackage _ class1) (WithoutPackage class2) = class1 == class2
    checkClassType (WithoutPackage class1) (WithPackage _ class2) = class1 == class2
    checkClassType (WithoutPackage class1) (WithoutPackage class2) = class1 == class2

-- | This function returns a primitve as a ref type (i.e. boxed primitve)
primToRefType :: PrimType -> RefType
primToRefType = toRefHelper
  where
    toRefHelper BooleanT = stringToRef "Boolean"
    toRefHelper ByteT = stringToRef "Byte"
    toRefHelper ShortT = stringToRef "Short"
    toRefHelper IntT = stringToRef "Integer"
    toRefHelper LongT = stringToRef "Long"
    toRefHelper CharT = stringToRef "Char"
    toRefHelper FloatT = stringToRef "Float"
    toRefHelper DoubleT = stringToRef "Double"

    stringToRef :: String -> RefType
    stringToRef x = ClassRefType (WithPackage refPackage [(Ident x, [])])
    refPackage :: Package
    refPackage = FullQualiPackage (map Ident ["java", "lang"])

-- | This function returns an Ident as a Type
withPackageIdentToType :: [Ident] -> Ident -> Type
withPackageIdentToType packages ident = RefType (ClassRefType (WithPackage (FullQualiPackage packages) [(ident, [])]))

-- | This function returns an Ident as a Type
withoutPackageIdentToType :: Ident -> Type
withoutPackageIdentToType ident = RefType (ClassRefType (WithoutPackage [(ident, [])]))

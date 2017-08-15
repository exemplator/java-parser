{-# LANGUAGE FlexibleInstances #-}
module Arbitrary where

import           Control.Monad         (replicateM)
import           Language.Java.Java
import           Language.Java.Parser
import           Test.Tasty.QuickCheck

----- Nodes
instance Arbitrary (CompilationUnitNode Singleton) where
    arbitrary = CompilationUnitNode <$> arbitrary
instance Arbitrary (TypeDeclNode Singleton) where
    arbitrary = ClassTypeDeclNode <$> arbitrary
instance Arbitrary (ClassDeclNode Singleton) where
    arbitrary = ClassDeclNode <$> arbitrary

----- Elements

instance Arbitrary (CompilationUnit Singleton) where
    arbitrary = CompilationUnit <$> arbitrary <*> arbitrary <*> arbitrary <*> ((:[]) <$> arbitrary)
instance Arbitrary (PackageDecl Singleton) where
    arbitrary = PackageDecl <$> arbitrary <*> arbitrary
instance Arbitrary (ImportDecl Singleton) where
    arbitrary = ImportDecl <$> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary (ClassDecl Singleton) where
    arbitrary = ClassDecl <$> arbitrary <*> pure [] <*> arbitrary <*> pure [] <*> pure Nothing <*> pure [] <*> arbitrary
instance Arbitrary (ClassBody Singleton) where
    arbitrary = ClassBody <$> arbitrary <*> pure []
instance Arbitrary Name where
    arbitrary = Name <$> (choose (1,3) >>= \len -> replicateM len arbitrary)
instance Arbitrary Ident where
    arbitrary = Ident . unkeyword <$> (choose (1,15) >>= \len -> replicateM len (elements (['a'..'z'] ++ ['A'..'Z'])))
      where unkeyword k
                | k `elem` ["if","do","then","else"] = "x" ++ k
                | otherwise                          = k


instance Arbitrary Package where
    arbitrary = FullQualiPackage <$> ((:[]) <$> arbitrary)

instance Arbitrary RefType where
    arbitrary = ClassRefType <$> arbitrary

instance Arbitrary ClassType where
    arbitrary = WithPackage <$> arbitrary <*> arbitrary

instance Arbitrary ClassName where
    arbitrary = ClassName <$> ((:[]) <$> arbitrary)

instance Arbitrary TypeArgument where
    arbitrary = ActualType <$> arbitrary

instance Arbitrary Singleton where
    arbitrary = return Singleton

data Singleton = Singleton
    deriving (Show, Eq)

instance Parsable Singleton where
    toParser a = a <*> (return Singleton)

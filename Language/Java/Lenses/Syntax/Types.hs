{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Lenses.Syntax.Types where

import           Language.Java.Prisms.CustomLenses
import           Language.Java.Syntax.Types

allLenses ''Type

allLenses ''RefType

allLenses ''ClassName

allLenses ''Package

allLenses ''TypeArgument

allLenses ''TypeDeclSpecifier

allLenses ''WildcardBound

allLenses ''PrimType

allLenses ''TypeParam

allLenses ''Ident

allLenses ''Name

allLenses ''RelaxedType

{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Prisms.Syntax.Types where

import           Control.Lens
import           Language.Java.Syntax.Types

makeLenses ''Type

makeLenses ''RefType

makeLenses ''ClassName

makeLenses ''Package

makeLenses ''TypeArgument

makeLenses ''TypeDeclSpecifier

makeLenses ''WildcardBound

makeLenses ''PrimType

makeLenses ''TypeParam

makeLenses ''Ident

makeLenses ''Name

makeLenses ''RelaxedType

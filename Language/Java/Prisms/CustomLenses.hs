module Language.Java.Prisms.CustomLenses where

import           Control.Lens
import           Language.Haskell.TH

allLenses :: Name -> DecsQ
allLenses = makeLensesWith $ lensRules
    & lensField .~ \_ _ name -> [TopName (mkName $ nameBase name)]

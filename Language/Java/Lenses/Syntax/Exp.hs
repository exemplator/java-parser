{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Lenses.Syntax.Exp where

import           Language.Java.Prisms.CustomLenses
import           Language.Java.Syntax.Exp

allLenses ''Literal

allLenses ''Op

allLenses ''AssignOp

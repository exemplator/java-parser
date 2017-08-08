{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Prisms.Syntax.Exp where

import           Control.Lens
import           Language.Java.Syntax.Exp

makeLenses ''Literal

makeLenses ''Op

makeLenses ''AssignOp

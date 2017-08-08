module Language.Java.Helper
    ( desugarAnnotation
    , desugarAnnotation'
    ) where

import           Language.Java.Syntax

-----------------------------------------------------------------------
-- Functions

desugarAnnotation :: Annotation l -> (Name, [(Ident, ElementValue l)])
desugarAnnotation (MarkerAnnotation n)          = (n, [])
desugarAnnotation (SingleElementAnnotation n e) = (n, [(Ident "value", e)])
desugarAnnotation (NormalAnnotation n kv)       = (n, kv)
desugarAnnotation' :: Annotation l -> Annotation l
desugarAnnotation' = uncurry NormalAnnotation . desugarAnnotation
{-# LANGUAGE DeriveGeneric #-}
module Language.Java.Position where

import           GHC.Generics    (Generic)
import           Text.Parsec.Pos hiding (Column, Line)

type Line = Int
type Column = Int

-- | Position in a Text Line Column
data Position = Position Line Column
    deriving (Show, Read, Eq, Generic)

type Start = Position
type End = Position

-- | A segment has a Start and an End
data Segment = Segment Start End
    deriving (Show, Read, Eq, Generic)

sourcePosToSegment :: SourcePos -> SourcePos -> Segment
sourcePosToSegment start end = Segment (toPostion start) (toPostion end)
    where
        toPostion sourcePos = Position (sourceLine sourcePos) (sourceColumn sourcePos)

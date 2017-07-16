{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Java.Position where

import           Data.Data
import           GHC.Generics    (Generic)
import           Text.Parsec.Pos hiding (Column, Line)

type Line = Int
type Column = Int

-- | Position in a Text Line Column
data Position = Position Line Column
    deriving (Eq,Show,Read,Typeable,Generic,Data)

type Start = Position
type End = Position

-- | A segment has a Start and an End
data Segment = Segment Start End
    deriving (Eq,Show,Read,Typeable,Generic,Data)

instance HasSegment Segment where
    getSegment = id

class HasSegment a where
    getSegment :: a -> Segment

sourcePosToSegment :: SourcePos -> SourcePos -> Segment
sourcePosToSegment start end = Segment (toPostion start) (toPostion end)
    where
        toPostion sourcePos = Position (sourceLine sourcePos) (sourceColumn sourcePos)

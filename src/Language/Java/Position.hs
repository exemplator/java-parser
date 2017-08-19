{-# LANGUAGE DeriveGeneric #-}
module Language.Java.Position where

import           Data.List
import           Data.Maybe
import           GHC.Generics        (Generic)
import           Text.Megaparsec.Pos

type Line = Int
type Column = Int

-- | Position in a Text Line Column
data Position = Position Line Column
    deriving (Show, Read, Eq, Generic)

type Start = Position
type End = Position

-- | A segment has a Start and an End
data Segment = Segment Start End
    deriving (Eq, Generic)

defaultSeg = Segment (Position (-1) (-1)) (Position (-1) (-1))

instance Ord Segment where
    compare seg1 seg2 =
        fromMaybe EQ $ find (/=EQ) $ zipWith compare [xa,xb,xc,xd] [ya,yb,yc,yd]
        where
            (xa, xb, xc, xd) = extract seg1
            (ya, yb, yc, yd) = extract seg2

instance Show Segment where
    show seg = shp sl sc ++
        if (sl,sc) == (el,ec) then "" else shp el ec
        where
            shp :: Int -> Int -> String
            shp l c = "(" ++ show l ++ "," ++ show c ++ "):"
            (sl, sc, el, ec) = extract seg

extract :: Segment -> (Line, Column, Line, Column)
extract (Segment (Position xa xb) (Position xc xd)) = (xa, xb, xc, xd)
{-# INLINE extract #-}

data Located e = Loc
        { locSpan :: Segment
        , locNode :: e
        }
        deriving (Generic)

instance Eq (Located e) where
        Loc x _  == Loc y _ = x == y

instance Ord (Located e) where
        compare (Loc x _) (Loc y _) = compare x y

instance Show e => Show (Located e) where
        show (Loc p e) = pS ++ sep ++ show e
            where
                pS :: String
                pS = show p
                sep :: String
                sep = "\n "

sourcePosToSegment :: SourcePos -> SourcePos -> Segment
sourcePosToSegment start end = Segment (toPostion start) (toPostion end)
    where
        toPostion sourcePos = Position ((unPos . sourceLine) sourcePos) ((unPos . sourceColumn) sourcePos)

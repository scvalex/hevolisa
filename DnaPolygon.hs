module DnaPolygon where

import Settings
import Tools
import DnaBrush
import DnaPoint

-- |A polygon has a brush for color and a list of points
data DnaPolygon = DnaPolygon DnaBrush [DnaPoint] deriving (Show)

-- |Get the brush of the polygon
polygonBrush :: DnaPolygon -> DnaBrush
polygonBrush (DnaPolygon brush _) = brush

-- |Get the points of the polygon
polygonPoints :: DnaPolygon -> [DnaPoint]
polygonPoints (DnaPolygon _ points) = points

-- |Initialize the polygon with random garbage
initPolygon :: IO DnaPolygon
initPolygon = do points <- randomPoints activePointsPerPolygonMin
                 brush <- initBrush
                 return (DnaPolygon brush points)

-- |Create a list of random points
randomPoints :: Integer       -- ^ Number of points
             -> IO [DnaPoint] -- ^ return the result
randomPoints n = do origin <- initPoint
                    points <- sequence $ replicate (fromIntegral n) (randomPoint origin)
                    return points

-- |Create a random point using another point
randomPoint :: DnaPoint -> IO DnaPoint
randomPoint (DnaPoint x y) = do x <- mutateDim 3 maxWidth x
                                y <- mutateDim 3 maxHeight y
                                return (DnaPoint x y)

-- |A polygon has mutable DNA
instance Mutable DnaPolygon where
    mutate = mutatePolygon

-- |Mutate a polygon by adding and removing points and other funny tricks
mutatePolygon :: DnaPolygon -> IO DnaPolygon
mutatePolygon p = maybeAddPoint p >>= maybeRemovePoint >>= mutateBrushInP >>= mutatePoints

-- |Coming soon...
maybeAddPoint = undefined
maybeRemovePoint = undefined
mutateBrushInP = undefined
mutatePoints = undefined
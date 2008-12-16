module DnaDrawing where

import DnaPolygon ( DnaPolygon, polygonPointsCount, initPolygon )
import Tools ( getRandomNumber )
import Settings

data DnaDrawing = DnaDrawing [DnaPolygon] deriving (Show)

drawingPolygons :: DnaDrawing -> [DnaPolygon]
drawingPolygons (DnaDrawing pls) = pls

drawingPolygonsCount :: Integral a => DnaDrawing -> a
drawingPolygonsCount = fromIntegral . length . drawingPolygons

drawingPointCount :: DnaDrawing -> Integer
drawingPointCount = sum . map polygonPointsCount . drawingPolygons

mapseq :: Monad m => m a -> [(a -> m a)]-> m a
mapseq f []     = f
mapseq s (f:fs) = mapseq (s >>= f) fs

initDrawing :: IO DnaDrawing
initDrawing = do polygons <- mapseq (sequence [initPolygon]) 
                             (replicate (fromIntegral activePolygonsMin) addPolygon)
                 return (DnaDrawing polygons)

insertPolygon :: Int -> DnaPolygon -> [DnaPolygon] -> [DnaPolygon]
insertPolygon index new lst = left ++ [new] ++ right
    where left = take index lst
          right = drop index lst

addPolygon :: [DnaPolygon] -> IO [DnaPolygon]
addPolygon ps = do random <- getRandomNumber 0 (length ps)
                   polygon <- initPolygon
                   return (insertPolygon random polygon ps)
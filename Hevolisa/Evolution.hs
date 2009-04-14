--
-- Module      : Evolution
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Evolution where

import Control.Concurrent.Chan
import Data.Function ( on )
import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Tools
import Hevolisa.Renderer
import Hevolisa.Settings

type Delta = Integer

-- | Context contains the current drawing and the source image for comparison
data EvolutionContext = EvolutionContext {
      drawing :: DnaDrawing,
      image   :: [Int],
      delta   :: Delta,
      improvement :: Delta, -- ^ how much better did we get
      width   :: Int,
      height  :: Int
} deriving (Show, Eq)

instance MutableImageInfo EvolutionContext where
    getWidth = width
    getHeight = height

instance Ord EvolutionContext where
    compare = compare `on` delta

data EvolutionSettings = EvolutionSettings {
      setGenerationSize :: Integer
}

-- | Init the context with image and initial drawing
initContext :: [Int] -> Int -> Int -> IO EvolutionContext
initContext image w h = do
  drawing <- randomInit (EvolutionContext blankDrawing [] (-1) 0 w h)
  return $ EvolutionContext drawing image (-1) 0 w h

-- | Start the evolution process
evolve :: EvolutionSettings -> Chan EvolutionContext -> Int -> Int -> [Int] -> IO ()
evolve sets gens w h srf = do
  c <- updateDelta =<< initContext srf w h
  iter sets gens 0 c

-- | Recursive function combines mutation and writing files
iter :: EvolutionSettings -> Chan EvolutionContext -> Int -> EvolutionContext -> IO ()
iter sets gens n ec = do 
  writeChan gens ec
  ec' <- return . minimum =<< mapM mutateEvolutionContext 
                              (replicate (fromIntegral $ setGenerationSize sets) ec)
  iter sets gens (n + 1) $ if delta ec' < delta ec 
                           then ec' 
                           else ec { improvement = 0 } -- no change => no improvement

-- | Color error, smaller is better
updateDelta :: EvolutionContext -> IO EvolutionContext
updateDelta ec = do
  d' <- drawingDelta (drawing ec) (width ec) (height ec) (image ec)
  return ec { delta = d', improvement = delta ec - d' }

-- | Mutate the drawing in the EvolutionContext
mutateEvolutionContext :: EvolutionContext -> IO EvolutionContext
mutateEvolutionContext ec = do 
  d' <- mutate ec (drawing ec)
  ec' <- updateDelta ec { drawing = d' }
  return ec'

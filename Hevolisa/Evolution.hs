--
-- Module      : Evolution
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Evolution where

import Control.Concurrent.Chan
import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Tools
import Hevolisa.Renderer ( drawingDelta, withImageFromPNG )
import Hevolisa.Settings

type Delta = Integer

-- | Context contains the current drawing and the source image for comparison
data EvolutionContext = EvolutionContext {
      drawing :: DnaDrawing,
      image   :: [Int],
      delta   :: Delta,
      width   :: Int,
      height  :: Int
} deriving (Show, Eq)

instance MutableImageInfo EvolutionContext where
    getWidth = width
    getHeight = height

data EvolutionParameters = EvolutionParameters
    { eoResize :: Float
    }

-- | Init the context with image and initial drawing
initContext :: ([Int], Int, Int)  -> IO EvolutionContext
initContext (image, w, h) = do
  drawing <- randomInit (EvolutionContext blankDrawing [] (-1) w h)
  return $ EvolutionContext drawing image (-1) w h

-- | Start the evolution process
evolve :: EvolutionParameters -> Chan EvolutionContext -> FilePath -> IO ()
evolve _ gens fp = do 
  c <- withImageFromPNG fp initContext 
  c <- updateDelta c
  iter gens 0 c

-- | Recursive function combines mutation and writing files
iter :: Chan EvolutionContext -> Int -> EvolutionContext -> IO ()
iter gens n ec = do 
  writeChan gens ec
  ec' <- mutateEvolutionContext ec
  ec' <- updateDelta ec'
  iter gens (n + 1) $ if delta ec' < delta ec then ec' else ec

-- | Color error, smaller is better
updateDelta :: EvolutionContext -> IO EvolutionContext
updateDelta ec = do
  d' <- drawingDelta (drawing ec) (width ec) (height ec) (image ec)
  return ec { delta = d' }

-- | Mutate the drawing in the EvolutionContext
mutateEvolutionContext :: EvolutionContext -> IO EvolutionContext
mutateEvolutionContext ec = do 
  d' <- mutate ec (drawing ec)
  return $ ec { drawing = d' }

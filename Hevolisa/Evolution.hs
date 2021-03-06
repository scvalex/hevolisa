--
-- Module      : Evolution
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Evolution where

import Control.Concurrent
import Control.Monad
import Data.Function ( on )
import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Tools
import Hevolisa.Renderer
import Hevolisa.Settings

type Delta = Integer

-- | Context contains the current drawing and the source image for comparison
data EvolutionContext = EvolutionContext 
    { drawing :: DnaDrawing
    , image   :: [Int]
    , delta   :: Delta
    , improvement :: Delta -- ^ how much better did we get
    , width   :: Int
    , height  :: Int
    } deriving (Show, Eq)

instance MutableImageInfo EvolutionContext where
    getWidth = width
    getHeight = height

instance Ord EvolutionContext where
    compare = compare `on` delta

data EvolutionSettings = EvolutionSettings 
    { setGenerationSize :: Integer
    , setLineageLength :: Integer
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
  iter sets gens c

iter :: EvolutionSettings -> Chan EvolutionContext -> EvolutionContext -> IO ()
iter sets gens ec = do 
  writeChan gens ec
  ec' <- return . minimum =<< mapM takeMVar =<< replicateM 2 (lineage sets (setLineageLength sets) ec)
  iter sets gens $ if delta ec' < delta ec 
                   then ec' { improvement = delta ec - delta ec'}
                   else ec { improvement = 0 } -- no change => no improvement

lineage :: EvolutionSettings
        -> Integer          -- ^ number of generations still to do in lineage
        -> EvolutionContext -- ^ image that we're working on and assorted data
        -> IO (MVar EvolutionContext)
lineage sets n ec = do
  o <- newEmptyMVar
  forkIO $ go o n ec
  return o
      where
        go o 0 ec = putMVar o ec
        go o n ec = do
          ec' <- return . minimum =<< mapM mutateEvolutionContext 
                                     (replicate (fromIntegral $ setGenerationSize sets) ec)
          go o (n - 1) $ if delta ec' < delta ec 
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

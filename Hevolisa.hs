--
-- Module      : Hevolisa
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Main where

import Prelude hiding ( catch )

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Graphics.Rendering.Cairo as C
import Hevolisa.Evolution
import Hevolisa.Renderer
import Hevolisa.Shapes.DnaDrawing ( DnaDrawing )
import System.Console.GetOpt
import System.Directory
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStrLn, stderr, stdout )
import Text.Printf ( printf )

data Flag = Help 
          | Resize String
            deriving Eq

data Options = Options
    { optHelp :: Bool
    , optResize :: Double
    , optShowGen :: Bool
    , optSampleSize :: Double
    , optWriteInterval :: Integer
    , optGenerationSize :: Integer
    , optLineageLength :: Integer
    } deriving ( Show )

defaultOptions = Options
                 { optHelp = False
                 , optResize = 1.0
                 , optSampleSize = 1.0
                 , optShowGen = False
                 , optWriteInterval = 1000
                 , optGenerationSize = 1
                 , optLineageLength = 5
                 }

options :: [OptDescr (Options -> Options)]
options = [ Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True } ))
                   "Show this help message"
          , Option [] ["resize"] (ReqArg (\r opts -> opts { optResize = read r } ) "ratio")
                   "Resize the output images to <ratio> times the original"
          , Option [] ["show-generation"] (NoArg (\opts -> opts { optShowGen = True} ))
                   "Show the generation in the top-left corner of the images produced"
          , Option [] ["sample-size"] (ReqArg (\r opts -> opts { optSampleSize = read r} ) "ratio")
                   "Scale the image down internally; increases speed but hurts output quality"
          , Option [] ["write-interval"] (ReqArg (\r opts -> opts { optWriteInterval = read r } ) "interval")
                   "Write an image every <interval> generations"
          -- FIXME: how do you signal errors with getopt?
          , Option [] ["generation-size"] (ReqArg (\r opts -> let rr = read r
                                                              in if rr < 1 then error "Less than one individual cannot be reproduced"
                                                                           else opts { optGenerationSize = rr } ) "size")
                   "How many individuals per generation"
          , Option [] ["lineage-length"] (ReqArg (\r opts -> let rr = read r
                                                             in if rr < 1 then error "How exactly could you habe a lineage of less than one?"
                                                                          else opts { optLineageLength = rr } ) "length")
                   "Number of generations lineages are allowed to diverge before being remerged"
          ]

main :: IO ()
main = do
  argv <- getArgs
  Just (os, fs) <- parseOpts argv
  case (os, fs) of
    (opts, files)
        | optHelp opts        -> help
        | not $ null files    -> start opts (head files) `catch` somethingErred "Main"
        | otherwise           -> help
  return ()
    where 
      parseOpts argv = case getOpt Permute options argv of
                         (opts, files, []) -> return $ Just (foldl (flip id) defaultOptions opts, files) 
                         (_, _, errs)      -> die errs >> return Nothing
      header = "Usage: hevolisa PNGFILE"
      info = usageInfo header options
      dump = hPutStrLn stderr
      die errs = dump (concat errs ++ info) >> exitFailure
      help = dump info                      >> exitSuccess

data Evolver = Evolver
    { echan :: Chan EvolutionContext
    , sourceWidth :: Int
    , sourceHeight :: Int
    }

start :: Options -> FilePath -> IO ()
start opts path = do
  e <- startEvolution EvolutionSettings { setGenerationSize = optGenerationSize opts
                                        , setLineageLength = optLineageLength opts 
                                        }
  let opts' = opts { optResize = optResize opts * (1.0 / optSampleSize opts) }
  let loop i imprv = do
        g <- readChan (echan e)
        printf "Generation %d: d= %d; s= %d d/g\n" i (delta g) (imprv `div` i)
        if optResize opts' == 1.0
          then maybeWriteToFile i (drawing g) (width g) (height g)
          else do
            let f = optResize opts'
                rd = resizeDrawing f $ drawing g
                rw = round $ (fromIntegral $ width g) * f
                rh = round $ (fromIntegral $ height g) * f
            maybeWriteToFile i rd rw rh
        if i == 1
           then loop 2 0 -- no improvement on first generation
           else loop (i+1) (imprv + improvement g)
  loop 1 0 -- first generation zero results in divide by zero exception
      where 
        maybeWriteToFile :: Integer -> DnaDrawing -> Int -> Int -> IO ()
        maybeWriteToFile n d w h
            | isTimeToWrite n = drawingToFile d w h n ( optShowGen opts )
            | otherwise       = return ()
        isTimeToWrite :: Integer -> Bool
        isTimeToWrite n = n `mod` (optWriteInterval opts) == 0
        startEvolution sets = do
                    fileExists <- doesFileExist path
                    unless fileExists $ error $ "File does not exist: " ++ path
                    gens <- newChan
                    printf "Hevolisa evolving %s...\n" path
                    (w, h) <- C.withImageSurfaceFromPNG path $ \srf -> do
                                w <- C.imageSurfaceGetWidth srf
                                h <- C.imageSurfaceGetHeight srf
                                srf' <- C.createImageSurface C.FormatRGB24
                                                             (round $ fromIntegral w * (optSampleSize opts))
                                                             (round $ fromIntegral h * (optSampleSize opts))
                                C.renderWith srf' $ do
                                         C.scale (optSampleSize opts) (optSampleSize opts)
                                         C.setSourceSurface srf 0 0
                                         C.paint
                                us' <- unpackSurface srf'
                                w' <- C.imageSurfaceGetWidth srf'
                                h' <- C.imageSurfaceGetHeight srf'
                                forkIO $ evolve sets gens w' h' us' `catch` somethingErred "Evolve"
                                return (w', h')
                    return $ Evolver gens w h


somethingErred :: String -> SomeException -> IO ()
somethingErred s e = do
  printf "%s process failed with:\n" s
  printf "\t %s\n" (show e)
  exitFailure
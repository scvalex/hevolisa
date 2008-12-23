--
-- Module      : Main
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Main where

import DnaDrawing
import Tools (randomInit, mutate)

generations = 10000

main :: IO ()
main = (foldl (>>=) randomInit (replicate generations mutate) :: IO DnaDrawing) >>= 
       putStrLn . show
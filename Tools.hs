--
-- Module      : Tools
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Tools (
              Mutable (mutate),
              Points (pointCount),
              RandomInit (randomInit),

              maybeMutate,
              getRandomNumber,
              mapseq,

              -- List index functions
              addElem,
              removeElem,
              moveElem
) where

import Random

-- |Decide whether it`s time for a mutation
willMutate :: Integer  -- ^ Mutation rate 
           -> IO Bool  -- ^ True: Mutate
willMutate mutationRate = do k <- getRandomNumber 0 mutationRate
                             return (k == 1)

-- |Get a random number between min and max
getRandomNumber :: Random a => 
                   a     -- ^ Minimum
                -> a     -- ^ Maximum
                -> IO a  -- ^ Random number action
getRandomNumber x y = getStdRandom (randomR (x,y))

-- |Perform a mutation action when it`s time to do so
maybeMutate :: Integer  -- ^ Mutation rate
            -> IO a     -- ^ Mutation action
            -> a        -- ^ Unchanged value to pass through
            -> IO a     -- ^ Composed action
maybeMutate rate action unchanged = do mutate <- willMutate rate
                                       if mutate then action else return unchanged

-- |Move a list element from index to index
moveElem :: Int -- ^ From index
         -> Int -- ^ To index
         -> [a] -> [a]
moveElem from to lst = addElem elem to $ removeElem from lst
    where elem = lst !! from


-- |Remove an item at index position from list
removeElem :: Int -> [a] -> [a]
removeElem n lst = left ++ right
    where left = take n lst
          right = drop (n + 1) lst

-- |Add an item at index position to list
addElem :: a -> Int -> [a] -> [a]
addElem item index lst = left ++ [item] ++ right
    where left = take index lst
          right = drop index lst

-- |Instances of Mutable can mutate their DNA
class Mutable a where
    -- |Perform a genetic mutation in a random way
    mutate :: a -> IO a

-- |Count the points
class Points a where
    -- |Count the points
    pointCount :: (Integral b) => a -> b

class RandomInit a where
    randomInit :: IO a

-- |Sequences functions that produce actions
mapseq :: Monad m => m a -> [a -> m a] -> m a
mapseq f []     = f
mapseq s (f:fs) = mapseq (s >>= f) fs
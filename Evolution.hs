module Evolution where

import Prelude hiding (error)
import DnaDrawing
import Tools

data EvolutionContext = EvolutionContext DnaDrawing SourceColorMatrix
                        deriving (Show,Eq)
data SourceColorMatrix = SourceColorMatrix deriving (Show,Eq)

generations = 10000

initContext :: SourceColorMatrix -> IO EvolutionContext
initContext s = initDrawing >>= \d -> return (EvolutionContext d s)

start s = mapseq (initContext s) (replicate generations step)

-- |Smaller is better
error :: EvolutionContext -> Double
error = undefined

-- |EvolutionContext mutates minimizing the error
instance Mutable EvolutionContext where
    mutate = step

-- |Single evolution step, minimize error
step :: EvolutionContext -> IO EvolutionContext
step ec = mutateEvolutionContext ec >>= \next -> return (min ec next)

-- |Mutate the drawing in the EvolutionContext
mutateEvolutionContext :: EvolutionContext -> IO EvolutionContext
mutateEvolutionContext (EvolutionContext d s) = do m <- mutate d
                                                   return (EvolutionContext m s)

-- |Compare EvolutionContext by error
instance Ord EvolutionContext where
    compare x y
        | error x == error y = EQ
        | error x <= error y = LT
        | otherwise          = GT
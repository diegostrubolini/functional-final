module MapReduceOperations where

import Parallel.MapReduce.Simple (run,distribute,lift,(>>=))
import Prelude hiding ((>>=))
import IMDbMovie


-- Top Rated Movies by Year

topRatedMapReduce :: Int -> [Movie] -> [(String, [Movie])] 
topRatedMapReduce n state = run mr state
        where
        mr = distribute n >>= lift topRatedmapper >>= lift topRatedReducer 
        
topRatedmapper :: [Movie] -> [(Movie, String)]
topRatedmapper [] = []
topRatedmapper (x:xs) = [(x, getYear x)] ++ topRatedmapper xs

topRatedReducer :: [Movie] -> [(String, [Movie])]
topRatedReducer [] = []
topRatedReducer xs = [(getYear (head xs), getTopRated xs)]
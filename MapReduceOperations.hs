module MapReduceOperations where

import Parallel.MapReduce.Simple (run,distribute,lift,(>>=))
import Prelude hiding ((>>=))
import IMDbMovie


-- Top Rated Movies by Year

topRatedMapReduce :: Int -> [Movie] -> [(String, [Movie])] 
topRatedMapReduce n state = run mr state
        where
        mr = distribute n >>= lift topRatedMapper >>= lift topRatedReducer 
        
topRatedMapper :: [Movie] -> [(Movie, String)]
topRatedMapper [] = []
topRatedMapper (x:xs) = [(x, getYear x)] ++ topRatedMapper xs

topRatedReducer :: [Movie] -> [(String, [Movie])]
topRatedReducer [] = []
topRatedReducer xs = [(getYear (head xs), topRated xs)]

-- Top Rated Movies by Year

popularDirectorsMapReduce :: Int -> [Movie] -> [(String, Int)] 
popularDirectorsMapReduce n state = run mr state
        where
        mr = distribute n >>= lift popularDirectorsMapper >>= lift popularDirectorsReducer 

popularDirectorsMapper :: [Movie] -> [(Movie, String)]
popularDirectorsMapper [] = []
popularDirectorsMapper (x:xs) = [(x, getDirector x)] ++ popularDirectorsMapper xs

popularDirectorsReducer :: [Movie] -> [(String, Int)]
popularDirectorsReducer [] = []
popularDirectorsReducer xs = [(getDirector (head xs), totalIMDbVotes xs)]
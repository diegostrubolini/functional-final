module MapReduceOperations where

import Parallel.MapReduce.Simple (run,distribute,lift,(>>=))
import Data.Binary
import Control.DeepSeq
import Prelude hiding ((>>=))
import IMDbMovie


mapReduce :: (Eq c, Binary a, NFData b, NFData c, NFData d1, NFData d2) 
                 => Int -> [a] -> ([a] -> [(b, c)]) -> ([b] -> [(d1, d2)]) -> [(d1,d2)]
mapReduce n state mapper reducer = run mr state
        where
        mr = distribute n >>= lift mapper >>= lift reducer 

-- Top Rated Movies by Year

topRatedMovie :: Int -> [Movie] -> [(String, [Movie])] 
topRatedMovie n movies = mapReduce n movies topRatedMapper topRatedReducer
        
topRatedMapper :: [Movie] -> [(Movie, String)]
topRatedMapper [] = []
topRatedMapper (x:xs) = [(x, year x)] ++ topRatedMapper xs

topRatedReducer :: [Movie] -> [(String, [Movie])]
topRatedReducer [] = []
topRatedReducer xs = [(year (head xs), topRated xs)]

-- Top Rated Movies by Year

popularDirectors:: Int -> [Movie] -> [(String, Int)] 
popularDirectors n movies = mapReduce n movies popularDirectorsMapper popularDirectorsReducer

popularDirectorsMapper :: [Movie] -> [(Movie, String)]
popularDirectorsMapper [] = []
popularDirectorsMapper (x:xs) = [(x, director x)] ++ popularDirectorsMapper xs

popularDirectorsReducer :: [Movie] -> [(String, Int)]
popularDirectorsReducer [] = []
popularDirectorsReducer xs = [(director (head xs), totalIMDbVotes xs)]

-- Actors Couples

actorsCouple:: Int -> [Movie] -> [((Actor, Actor), [Movie])] 
actorsCouple n movies =  mapReduce n movies actorsCoupleMapper actorsCoupleReducer

actorsCoupleMapper :: [Movie] -> [((Movie, (Actor, Actor)), (Actor, Actor))]
actorsCoupleMapper [] = []
actorsCoupleMapper (x:xs) = map (\ac -> ((x, ac), ac)) (actorsCombination x) ++ actorsCoupleMapper xs

actorsCoupleReducer :: [(Movie, (Actor, Actor))] -> [((Actor, Actor), [Movie])]
actorsCoupleReducer [] = []
actorsCoupleReducer xs = [(snd (head xs), map fst xs)]
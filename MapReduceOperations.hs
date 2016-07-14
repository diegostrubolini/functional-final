module MapReduceOperations where

import Parallel.MapReduce.Simple (run,distribute,lift,(>>=))
import Data.Binary
import Data.Map (insertWith', empty, filter, elems, keys)
import Control.DeepSeq
import Prelude hiding ((>>=))
import IMDbMovie


mapReduce :: (Eq c, Binary a, NFData b, NFData c, NFData d1, NFData d2) 
                 => Int -> [a] -> ([a] -> [(b, c)]) -> ([b] -> [(d1, d2)]) -> [(d1,d2)]
mapReduce n state mapper reducer = run mr state
        where
        mr = distribute n >>= lift mapper >>= lift reducer 

-- Top Rated Movies by Year (The movies with best imdbRating for each Year)

topRatedMovie :: Int -> Int -> [Movie] -> [(String, [Movie])] 
topRatedMovie n year movies = mapReduce n (Prelude.filter (\m -> yearNum m >= year) movies) topRatedMapper topRatedReducer
        
topRatedMapper :: [Movie] -> [(Movie, String)]
topRatedMapper [] = []
topRatedMapper (x:xs) = [(x, year x)] ++ topRatedMapper xs

topRatedReducer :: [Movie] -> [(String, [Movie])]
topRatedReducer [] = []
topRatedReducer xs = [(year (head xs), topRated xs)]

-- Popular Directors (Directors whose total number of votes for every movie reaches a specific number)

popularDirectors:: Int -> Int -> [Movie] -> [(String, Int)] 
popularDirectors n votes movies = Prelude.filter(\(d,v) -> v >= votes) (mapReduce n movies popularDirectorsMapper popularDirectorsReducer)

popularDirectorsMapper :: [Movie] -> [((Movie,Director), Director)]
popularDirectorsMapper [] = []
popularDirectorsMapper (x:xs) = map (\d -> ((x, d), d)) (directorsList x) ++ popularDirectorsMapper xs

popularDirectorsReducer :: [(Movie, Director)] -> [(String, Int)]
popularDirectorsReducer [] = []
popularDirectorsReducer xs = [((snd (head xs)), totalIMDbVotes (map fst xs))]

-- Actors Couples (All the movies every couple of actors has worked in)

actorsCouple:: Int -> Int -> [Movie] -> [((Actor, Actor), [Movie])] 
actorsCouple n m movies =  Prelude.filter (\((a1,a2), ms) -> length ms >= m) (mapReduce n movies actorsCoupleMapper actorsCoupleReducer)

actorsCoupleMapper :: [Movie] -> [((Movie, (Actor, Actor)), (Actor, Actor))]
actorsCoupleMapper [] = []
actorsCoupleMapper (x:xs) = map (\ac -> ((x, ac), ac)) (actorsCombination x) ++ actorsCoupleMapper xs

actorsCoupleReducer :: [(Movie, (Actor, Actor))] -> [((Actor, Actor), [Movie])]
actorsCoupleReducer [] = []
actorsCoupleReducer xs = [(snd (head xs), map fst xs)]

-- Fetiche Actors (Actors that worked the highest number of times with each director)

feticheActors:: Int -> [Movie] -> [(Director, [Actor])] 
feticheActors n movies =  mapReduce n movies feticheActorsMapper feticheActorsReducer

feticheActorsMapper :: [Movie] -> [((Director, [Actor]), Director)]
feticheActorsMapper [] = []
feticheActorsMapper (x:xs) = map (\d->((d, actorsList x),d)) (directorsList x) ++ feticheActorsMapper xs

feticheActorsReducer :: [(Director, [Actor])] -> [(Director, [Actor])]
feticheActorsReducer [] = []
feticheActorsReducer xs = [(fst (head xs), mode (concat (map snd xs)))]


-- Code from https://rosettacode.org/wiki/Averages/Mode#Haskell

mode :: (Ord a) => [a] -> [a]
mode xs = keys (Data.Map.filter (== maximum (elems counts)) counts)
  where counts = foldr (\x -> insertWith' (+) x 1) empty xs
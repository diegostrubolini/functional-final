module IMDbMovie where

import Data.Binary
import Data.String.Utils
import Data.List
import Control.DeepSeq
import GHC.Generics

data Movie =
  Movie { title  :: String
         , year   :: String
         , director   :: String
         , genre   :: String
         , imdbRating   :: String
         , imdbVotes   :: String
         , product   :: String
         , poster   :: String
         , actors   :: String 
         } deriving (Generic)

type Actor = String

instance Binary Movie
instance NFData Movie

instance Show Movie where
  show movie = show (title movie)

-- Operations over a movie

imdbRatingNum :: Movie -> Float
imdbRatingNum movie = if (imdbRating movie) == "N/A" then 0 else read (imdbRating movie) :: Float

imdbVotesNum :: Movie -> Int
imdbVotesNum movie = if (imdbVotes movie) == "N/A" then 0 else read (replace "," "" (imdbVotes movie)) :: Int

actorsList :: Movie -> [Actor]
actorsList movie = if (actors movie) == "N/A" then [] else split ", " (actors movie)

isMovie :: Movie -> Bool
isMovie movie = (IMDbMovie.product movie) == "movie"

actorsCombination :: Movie -> [(Actor, Actor)]
actorsCombination movie = map orderActor [(x,y) | (x:ys) <- tails (actorsList movie), y <- ys]

orderActor :: (Actor, Actor) -> (Actor, Actor)
orderActor (a1, a2) = if a1 < a2 then (a1,a2) else (a2,a1)

-- Operations over movie list

topRated :: [Movie] -> [Movie]
topRated xs = filter (\m-> imdbRatingNum m == topRating) xs where topRating = (maximum (map imdbRatingNum xs))

totalIMDbVotes :: [Movie] -> Int
totalIMDbVotes xs = sum (map imdbVotesNum xs)

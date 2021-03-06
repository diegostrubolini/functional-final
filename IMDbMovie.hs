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
type Director = String

instance Binary Movie
instance NFData Movie

instance Show Movie where
  show movie = show (title movie)

-- Operations over a movie

imdbVotesNum :: Movie -> Int
imdbVotesNum movie = if (imdbVotes movie) == "N/A" then 0 else read (replace "," "" (imdbVotes movie)) :: Int

yearNum :: Movie -> Int
yearNum movie = if (year movie) == "N/A" then 0 else read (year movie) :: Int

actorsList :: Movie -> [Actor]
actorsList movie = if (actors movie) == "N/A" then [] else split ", " (actors movie)

directorsList :: Movie -> [Director]
directorsList movie = if (director movie) == "N/A" then [] else split ", " (director movie)

isMovie :: Movie -> Bool
isMovie movie = (IMDbMovie.product movie) == "movie"

actorsCombination :: Movie -> [(Actor, Actor)]
actorsCombination movie = map orderActor [(x,y) | (x:ys) <- tails (actorsList movie), y <- ys]

orderActor :: (Actor, Actor) -> (Actor, Actor)
orderActor (a1, a2) = if a1 < a2 then (a1,a2) else (a2,a1)

-- Operations over movie list

topRated :: [Movie] -> [Movie]
topRated xs = filter (\m-> imdbVotesNum m == topRatings) xs where topRatings = (maximum (map imdbVotesNum xs))

totalIMDbVotes :: [Movie] -> Int
totalIMDbVotes xs = sum (map imdbVotesNum xs)

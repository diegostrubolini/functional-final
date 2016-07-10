module IMDbMovie where

import Data.Binary
import Data.String.Utils
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

isMovie :: Movie -> Bool
isMovie movie = (IMDbMovie.product movie) == "movie"

-- Operations over movie list

topRated :: [Movie] -> [Movie]
topRated xs = filter (\m-> imdbRatingNum m == (maximum (map imdbRatingNum xs))) xs

totalIMDbVotes :: [Movie] -> Int
totalIMDbVotes xs = sum (map imdbVotesNum xs)

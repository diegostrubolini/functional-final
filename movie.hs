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
         } deriving (Generic)

instance Binary Movie
instance NFData Movie

instance Show Movie where
  show (Movie t y d g ir iv pr po) = show t

-- Operations over a movie

getTitle :: Movie -> String
getTitle (Movie t y d g ir iv pr po) = t

getYear :: Movie -> String
getYear (Movie t y d g ir iv pr po) = y

getImdbRating :: Movie -> Float
getImdbRating (Movie t y d g ir iv pr po) = read ir :: Float

getIMDbVotes :: Movie -> Int
getIMDbVotes (Movie t y d g ir iv pr po) = read (replace "," "" iv) :: Int

getDirector :: Movie -> String
getDirector (Movie t y d g ir iv pr po) = d

isMovie :: Movie -> Bool
isMovie (Movie t y d g ir iv pr po) = pr == "movie"

-- Operations over movie list

topRated :: [Movie] -> [Movie]
topRated xs = filter (\m-> getImdbRating m == (maximum (map getImdbRating xs))) xs

totalIMDbVotes :: [Movie] -> Int
totalIMDbVotes xs = sum (map getIMDbVotes xs)

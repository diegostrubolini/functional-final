module IMDbMovie where

import Data.Binary
import Control.DeepSeq
import GHC.Generics

data Movie =
  Movie { title  :: String
         , year   :: String
         , director   :: String
         , genre   :: String
         , imdbRating   :: String
         , product   :: String
         , poster   :: String
         } deriving (Generic)

instance Binary Movie
instance NFData Movie

instance Show Movie where
  show (Movie t y d g i pr po) = show t

getTitle :: Movie -> String
getTitle (Movie t y d g i pr po) = t

getYear :: Movie -> String
getYear (Movie t y d g i pr po) = y

getImdbRating :: Movie -> Float
getImdbRating (Movie t y d g i pr po) = read i :: Float

getTopRated :: [Movie] -> [Movie]
getTopRated xs = filter (\m-> getImdbRating m == (maximum (map getImdbRating xs))) xs

isMovie :: Movie -> Bool
isMovie (Movie t y d g i pr po) = pr == "movie"
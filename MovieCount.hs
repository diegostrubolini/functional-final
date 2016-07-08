-- | Classic word count MapReduce algorithm written with the monad
--
--   Takes as argument:
--
--   * The name of file of word-based text
--
--   * (Optional) the number of mappers to use in the first stage
--     (defaults to 16)
module Main where


import System.IO
import System.Environment (getArgs)
import Parallel.MapReduce.Simple (run,distribute,lift,(>>=))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Binary
import Control.DeepSeq
import Prelude hiding ((>>=))
import qualified Data.ByteString.Lazy.Char8 as B
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


instance ToJSON Movie
instance FromJSON Movie

main::IO()
main = do
        args <- getArgs
        out <- case length args of 
                0 -> error "Usage: moviecount [filename] ([num mappers])"
                _ -> do
                        let nMap = case length args of
                                1 -> 16
                                _ -> read $ args!!1
                        d <- (eitherDecode <$> getJSON (head args)) :: IO (Either String [Movie])
                        let res = case d of
                                Left err -> []
                                Right ps -> mapReduce nMap (filter isMovie ps)
                        return res
        print out

putLines :: FilePath -> [String] -> IO ()
putLines file text = do
        h <- openFile file WriteMode
        hPutStr h $ unwords text
        hClose h
        return () 


getJSON :: FilePath -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

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


mapReduce :: Int
        -> [Movie]   
        -> [(String, [Movie])] 
mapReduce n state = run mr state
        where
        mr = distribute n >>= lift topRatedmapper >>= lift topRatedReducer 

-- transformers
        
topRatedmapper :: [Movie] -> [(Movie, String)]
topRatedmapper [] = []
topRatedmapper (x:xs) = [(x, getYear x)] ++ topRatedmapper xs

topRatedReducer :: [Movie] -> [(String, [Movie])]
topRatedReducer [] = []
topRatedReducer xs = [(getYear (head xs), getTopRated xs)]




         


 
                      
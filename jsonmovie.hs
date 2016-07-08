module JSONMovie where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B
import IMDbMovie

instance ToJSON Movie
instance FromJSON Movie

getJSON :: FilePath -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

readMoviesFromJSON :: FilePath -> IO (Either String [Movie])
readMoviesFromJSON filePath = (eitherDecode <$> getJSON filePath)
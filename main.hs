module Main where


import System.IO
import System.Environment (getArgs)
import Parallel.MapReduce.Simple (run,distribute,lift,(>>=))
import Prelude hiding ((>>=))
import IMDbMovie
import JSONMovie

main::IO()
main = do
        args <- getArgs
        out <- case length args of 
                0 -> error "Usage: moviecount [filename] ([num mappers])"
                _ -> do
                        let nMap = case length args of
                                1 -> 16
                                _ -> read $ args!!1
                        d <- readMoviesFromJSON (head args)
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




         


 
                      
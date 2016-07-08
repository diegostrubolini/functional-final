module Main where


import System.IO
import System.Environment (getArgs)
import IMDbMovie
import JSONMovie
import MapReduceOperations

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
                        let movies = case d of
                                Nothing -> []
                                Just ms -> filter isMovie ms
                        return (topRatedMapReduce nMap movies)
        print out
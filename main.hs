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
                                2 -> 16
                                _ -> read $ args!!2
                        let option = case length args of
                                1 -> 1
                                _ -> read $ args!!1 
                        d <- readMoviesFromJSON (head args)
                        let movies = case d of
                                Nothing -> []
                                Just ms -> filter isMovie ms
                        let res = case option of
                                1 -> []
                                2 -> popularDirectorsMapReduce nMap movies
                                _ -> []
                        return res
        print out
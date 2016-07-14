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
                0 -> error "Usage: Main [filename] ([num mappers])"
                _ -> do
                        let option = case length args of
                                1 -> 1
                                _ -> read $ args!!1
                        let param = case length args of
                                1 -> -1
                                2 -> -1
                                _ -> read $ args!!2  
                        let nMap = case length args of
                                1 -> 16
                                2 -> 16
                                3 -> 16
                                _ -> read $ args!!3
                        d <- readMoviesFromJSON (head args)
                        let movies = case d of
                                Nothing -> []
                                Just ms -> filter isMovie ms
                        let res = case option of
                                1 -> printTopRated (case param of
                                        -1 -> topRatedMovie nMap 0 movies
                                        _ -> topRatedMovie nMap param movies)
                                2 -> printPopularDirectors (case param of
                                        -1 -> popularDirectors nMap 0 movies
                                        _ -> popularDirectors nMap param movies)
                                3 -> printActorsCouple (case param of
                                        -1 -> actorsCouple nMap 1 movies
                                        _ -> actorsCouple nMap param movies)
                                4 -> printFeticheActors (feticheActors nMap movies)
                                _ -> []
                        return res
        putStrLn "=============================="
        putStrLn "           Results            "
        putStrLn "=============================="
        putStrLn out
        putStrLn "=============================="

printTopRated:: [(String, [Movie])] -> String
printTopRated = foldr (\m ms -> "[" ++ (fst m) ++ "]" ++ "\n" ++  printMovies (snd m) ++ ms) ""

printPopularDirectors:: [(String, Int)] -> String
printPopularDirectors = foldr (\(n,v) ds ->  n ++ " : " ++ show v  ++ "\n"  ++ ds) ""

printActorsCouple:: [((Actor, Actor), [Movie])] -> String
printActorsCouple = foldr (\((a1,a2), ms) as ->  "[" ++ a1 ++ "|" ++ a2 ++ "]" ++ "\n" ++ printMovies ms ++ as) ""

printFeticheActors:: [(Director, [Actor])] -> String
printFeticheActors = foldr (\(d,as) fs ->  "[" ++ d ++ "] : " ++ printActors as ++ "\n"  ++ fs) ""

printMovies:: [Movie] -> String
printMovies =  foldr (\m ms -> show m ++ "\n" ++ ms) ""

printActors:: [Actor] -> String
printActors [] = ""
printActors [a] = a
printActors (a:as) =  a ++ " | " ++ printActors as



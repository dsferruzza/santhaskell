module Main where

import Santhaskell
import qualified Data.Set as Set
import System.Random
import System.Directory (doesFileExist)

main :: IO ()
main = do
    -- Get a set of people from the input file
    input <- readFile "input.txt"
    let l = filter (not.null) (lines input)
    let people = Set.map Person (Set.fromList l)

    -- Make a random draw
    gen <- getStdGen 
    let result = randomDraw people gen

    -- Display result
    putStrLn $ makeOutput result

    -- Write it to a file
    writeResult result

-- Write result to an output file
writeResult :: Set.Set RandomDrawResult -> IO ()
writeResult result = do
    year <- getCurrentYear
    let out = "output/" ++ year ++ ".txt"
    exists <- doesFileExist out
    if exists
        then do
            putStrLn $ "The file " ++ out ++ " already exists! Override it? [n]"
            confirm <- getLine
            if confirm == "y"
                then do
                    writeFileForReal out result
                else do
                    putStrLn $ out ++ " was left untouched"
        else do
            writeFileForReal out result

writeFileForReal :: FilePath -> Set.Set RandomDrawResult -> IO ()
writeFileForReal out result = do
    writeFile out (makeOutput result)
    putStrLn $ "Wrote result in file : " ++ out

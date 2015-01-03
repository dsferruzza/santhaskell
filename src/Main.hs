module Main where

import Santhaskell
import Parsers
import System.IO.UTF8
import Prelude hiding (readFile, writeFile, putStrLn, getLine)
import qualified Data.Set as Set
import Data.List
import System.Random
import System.Directory (doesFileExist, getDirectoryContents)
import Control.Monad (filterM, liftM, liftM2)
import Text.Regex.Posix ((=~))
import Text.Parsec.Error

inputFile :: FilePath
inputFile = "input.txt"

outputDir :: FilePath
outputDir = "output/"

main :: IO ()
main = do
    -- Get a set of people from the input file
    input <- readFile inputFile
    let parsedInput = parseInput input
    either parseError doRandomDraw parsedInput
    --let l = filter (not.null) (lines input)
    --let people = Set.map Person (Set.fromList l)

parseError :: ParseError -> IO ()
parseError e = do
    putStrLn $ "Can't parse input !\n" ++ (show e)

doRandomDraw :: [Person] -> IO ()
doRandomDraw l = do
    let people = Set.fromList l

    -- Get exisiting random draws
    --contents <- getExistingRandowDraws

    -- Make a random draw
    gen <- getStdGen 
    let result = randomDraw people gen

    -- Display result
    putStrLn ""
    putStrLn "Results:"
    putStrLn ""
    putStrLn $ makeOutput result

    -- Write it to a file
    writeResult result

getExistingRandowDraws :: IO [String]
getExistingRandowDraws = do
    directoryContent <- getDirectoryContents outputDir
    let matchPattern file = file =~ (outputDir ++ "[0-9]{4}[.]txt") :: Bool
    let hasValidPattern file = liftM matchPattern (return file) :: IO Bool
    let isValid file = liftM2 (&&) (doesFileExist file) (hasValidPattern file) :: IO Bool
    files <- filterM isValid $ fmap (outputDir ++) directoryContent :: IO [FilePath]
    let chosenFiles = take 2 $ reverse $ sort $ files
    putStrLn $ "Importing old results from " ++ (show chosenFiles)
    content <- sequence $ fmap (readFile) chosenFiles :: IO [String]
    return content

-- Write result to an output file
writeResult :: Set.Set RandomDrawResult -> IO ()
writeResult result = do
    year <- getCurrentYear
    let out = outputDir ++ year ++ ".txt"
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

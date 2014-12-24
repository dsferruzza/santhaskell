module Main where

import Santhaskell
import qualified Data.Set as Set
import System.Random

main :: IO ()
main = do
    input <- readFile "input.txt"
    let l = filter (not.null) (lines input)
    let people = Set.map Person (Set.fromList l)
    --putStrLn $ show people
    gen <- getStdGen 
    --putStrLn $ show $ shuffle (Set.toList people) gen
    let result = randomDraw people gen
    --putStrLn $ show result
    year <- getCurrentYear
    let out = "output/" ++ year ++ ".txt"
    putStrLn $ makeOutput result
    writeFile out (makeOutput result)
    putStrLn $ "Wrote result in file : " ++ out

module Main where

import Data.List
import qualified Data.Set as Set
import Data.Maybe
import System.Random
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

-- A person (which is just a name)
newtype Person = Person { getPerson :: String } deriving (Eq, Ord)
instance Show Person where show = getPerson

-- A person giving a gift to another person and receiving one from a third person
data RandomDrawResult = RandomDrawResult { person :: Person
                                         , giveTo :: Person
                                         , receiveFrom :: Person
                                         } deriving (Ord)
instance Eq RandomDrawResult where RandomDrawResult p1 _ _ == RandomDrawResult p2 _ _ = p1 == p2
instance Show RandomDrawResult where
    show (RandomDrawResult name to _) = (getPerson name) ++ " -> " ++ (getPerson to)

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

-- Change the order of items in a list
shuffle :: [a] -> StdGen -> [a]
shuffle list gen = head $ drop x possibilities
    where
        possibilities = permutations list
        n = length possibilities
        (x, _) = randomR (0, n - 1) gen

-- Tell who needs to offer a gift to who
randomDraw :: Set.Set Person -> StdGen -> Set.Set RandomDrawResult
randomDraw people gen = foldl transform Set.empty list
    where
        list = shuffle (Set.toList people) gen
        index p = fromJust $ elemIndex p list
        getNext p = (cycle list) !! ((index p) + 1)
        getPrevious p = (cycle list) !! ((index p) - 1 + (length list))
        transform acc cur = Set.insert (RandomDrawResult cur (getNext cur) (getPrevious cur)) acc

-- Output a set of random draw result in a pretty way
makeOutput :: Set.Set RandomDrawResult -> String
makeOutput result = unlines $ map show (Set.toList result)

-- Get the current year
getCurrentYear :: IO String
getCurrentYear = do
    now <- getCurrentTime
    return (formatTime defaultTimeLocale "%Y" now)

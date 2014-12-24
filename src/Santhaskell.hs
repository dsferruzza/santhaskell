module Santhaskell where

import Data.List
import qualified Data.Set as Set
import Data.Maybe
import System.Random
import Data.Array.ST (STArray, newListArray, readArray, writeArray)
import Control.Monad (liftM, forM)
import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
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

-- Randomly shuffle a list without the IO Monad /O(N)/
-- Inspired by https://www.haskell.org/haskellwiki/Random_shuffle
shuffle :: [a] -> StdGen -> [a]
shuffle xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        return xs')
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n'' xs'' = newListArray (1, n'') xs''

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

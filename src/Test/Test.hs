{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-orphans #-}
module Main where

import Test.Framework
import System.Random (mkStdGen)
import Data.List
import qualified Data.Set as Set

import Santhaskell
import Parsers

main :: IO ()
main = htfMain htf_thisModulesTests


-- Utils

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just


-- Instances

instance Arbitrary Person where
    arbitrary = do
        name <- arbitrary
        return $ Person name

instance (Arbitrary a, Ord a) => Arbitrary (Set.Set a) where
    arbitrary = do
        as <- arbitrary
        return $ Set.fromList as


-- Tests on Santhaskell.shuffle

prop_suffleGiveASimilarList :: [Person] -> Int -> Bool
prop_suffleGiveASimilarList l seed = (length l) == (length $ shuffle l (mkStdGen seed))

prop_shuffleKeepElements :: [Person] -> Int -> Bool
prop_shuffleKeepElements l seed = all (== True) hasElement
    where
        sl = shuffle l (mkStdGen seed)
        hasElement = map (\i -> elem i sl) l


-- Tests on Santhaskell.randomDraw

prop_randomDrawGivenOneResultPerPerson :: Set.Set Person -> Int -> Bool
prop_randomDrawGivenOneResultPerPerson people seed = (Set.size people) == (Set.size $ randomDraw people (mkStdGen seed))

prop_randomDrawResultIsConsistent :: Set.Set Person -> Int -> Bool
prop_randomDrawResultIsConsistent people seed
                                | Set.size people < 3 = True
                                | otherwise = Set.foldl (&&) True consistentResults
    where
        results = randomDraw people (mkStdGen seed)
        consistentResults = Set.map isConsistent results
        isConsistent r = (person r) /= (giveTo r) && (person r) /= (receiveFrom r) && (giveTo r) /= (receiveFrom r)

prop_randomDrawResultsFormACycle :: Set.Set Person -> Int -> Bool
prop_randomDrawResultsFormACycle people seed
                                | Set.size people < 3 = True
                                | otherwise = (Set.size people) == (length list)
    where
        results = Set.toList $ randomDraw people (mkStdGen seed)
        first = (head results, [])
        list = unfoldr handleNext first
        handleNext r = fmap (\i -> (person i, (i, (person i):(snd r)))) (next r)
        next (r, l) = find (\i -> person i == giveTo r) results >>= (\nextr -> if elem (person nextr) l then Nothing else Just nextr)


-- Tests on Parsers.parseInput

test_parseInputParsesCorrectNames :: IO ()
test_parseInputParsesCorrectNames = assertEqual (Just [Person "Name1", Person "NameWithAccentséêèâàîï", Person "Name3"]) (rightToMaybe $ parseInput "Name1\nNameWithAccentséêèâàîï\nName3")

test_parseInputPartiallyParsesNamesWithSpaces :: IO ()
test_parseInputPartiallyParsesNamesWithSpaces = assertEqual (Just [Person "Name"]) (rightToMaybe $ parseInput "Name with spaces")

test_parseInputPartiallyParsesNamesWithDashes :: IO ()
test_parseInputPartiallyParsesNamesWithDashes = assertEqual (Just [Person "NameWith"]) (rightToMaybe $ parseInput "NameWith-ADash")

{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import System.Random
import Santhaskell

main = htfMain htf_thisModulesTests

instance Arbitrary Person where
    arbitrary = do
        name <- arbitrary
        return $ Person name

prop_suffleGiveASimilarList :: [Person] -> Int -> Bool
prop_suffleGiveASimilarList l seed = (length l) == (length $ shuffle l (mkStdGen seed))

prop_shuffleKeepElements :: [Person] -> Int -> Bool
prop_shuffleKeepElements l seed = foldl (&&) True hasElement
    where
        sl = shuffle l (mkStdGen seed)
        hasElement = map (\i -> elem i sl) l

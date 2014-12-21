{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Test where

import Test.Framework
import System.Random
import Main

main = htfMain htf_thisModulesTests

instance Arbitrary Person where
	arbitrary = do
		name <- arbitrary
		return $ Person name

prop_shuffle :: [Person] -> Int -> Bool
prop_shuffle l seed = l /= shuffle l (mkStdGen seed)

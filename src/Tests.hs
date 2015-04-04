{-# LANGUAGE FlexibleInstances #-}

import           Sort
import           Test.QuickCheck       ()
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

setMaxSuccess :: TestTree -> TestTree
setMaxSuccess = localOption $ QuickCheckTests 1000

tests :: TestTree
tests = setMaxSuccess $ testGroup "Lists"
  [ QC.testProperty "sorting in ascending order" $ \lists ->
      let
        ascSorted = sortLists Asc lists
        ascOrdered = checkOrder (<=) $ listLengths ascSorted
      in
        ascOrdered

  , QC.testProperty "sorting in descending order" $ \lists ->
      let
        descSorted = sortLists Desc lists
        descOrdered = checkOrder (>=) $ listLengths descSorted
      in
        descOrdered

  , QC.testProperty "sorting with asc is the opposite of comparing with desc" $ \lists ->
      let
        ascSorted = sortLists Asc (lists :: [[Int]])
        descSorted = sortLists Desc lists
      in
        ascSorted == reverse descSorted
  ]

listLengths :: [List Int] -> [Int]
listLengths = fmap length

checkOrder :: (Int -> Int -> Bool) -> [Int] -> Bool
checkOrder comparePred lengths = ordered
  where
    neighbors = tail lengths
    ordered = and $ zipWith comparePred lengths neighbors

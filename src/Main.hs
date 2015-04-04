{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.List          (sortBy)
import           System.Environment (getArgs)

-- Manually set the expected input type as GHC needs to know reading from
-- the command-line. The exact type is irrelevant in this challenge though.
type InputType = Int

type List a = [a]

data Order = Asc | Desc

instance Read Order where
    readsPrec prec "asc"  = [(Asc, "")]
    readsPrec prec "desc" = [(Desc, "")]
    readsPrec prec _      = [(Asc, "")]

main :: IO ()
main = do
    args <- getArgs
    let order = read (if null args then "asc" else head args) :: Order

    line <- getLine
    let lists = read line :: [[InputType]]

    print . sortLists order $ lists

sortLists :: Order -> [List a] -> [List a]
sortLists order = sortBy (compareLists order)

-- This creates a compare predicate for sorting.
compareLists :: Order -> List a -> List a -> Ordering
compareLists Asc a b  = if compareLists' a b then LT else GT
compareLists Desc a b = if compareLists' a b then GT else LT

compareLists' :: List a -> List a -> Bool
compareLists' a b = length a < length b

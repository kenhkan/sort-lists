{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           System.Environment              (getArgs)
import Data.List (sortBy)

-- Manually set the expected input type, which is irrelevant in this
-- challenge anyway.
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
    let order = read (if (null args) then "asc" else head args) :: Order

    line <- getLine
    let lists = read line :: [[InputType]]

    let display = putStrLn . show
    display . sortLists order $ lists

sortLists :: Order -> [List a] -> [List a]
sortLists order lists = sortBy (compareLists order) lists

-- This creates a compare predicate for sorting.
compareLists :: Order -> (List a -> List a -> Ordering)
compareLists Asc a b  = if compareLists' a b then LT else GT
compareLists Desc a b = if compareLists' a b then GT else LT

compareLists' :: List a -> List a -> Bool
compareLists' a b = length a < length b

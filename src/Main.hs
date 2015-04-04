{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Sort
import           System.Environment (getArgs)

-- Manually set the expected input type as GHC needs to know reading from
-- the command-line. The exact type is irrelevant in this challenge though.
type InputType = Int

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

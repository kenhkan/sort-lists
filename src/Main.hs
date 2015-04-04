{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           System.Environment              (getArgs)

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
    -- Manually set the expected type, which is irrelevant in this
    -- challenge anyway.
    let lists = read line :: [[Int]]

    let display = putStrLn . show . concat
    display . sortLists order $ lists

sortLists :: Order -> [[a]] -> [[a]]
sortLists _ lists = lists

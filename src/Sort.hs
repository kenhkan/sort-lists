module Sort where

import           Data.List          (sortBy)

type List a = [a]

data Order = Asc | Desc

sortLists :: Order -> [List a] -> [List a]
sortLists order = sortBy (compareLists order)

-- This creates a compare predicate for sorting.
compareLists :: Order -> List a -> List a -> Ordering
compareLists Asc a b  = if compareLists' a b then LT else GT
compareLists Desc a b = if compareLists' a b then GT else LT

compareLists' :: List a -> List a -> Bool
compareLists' a b = length a < length b

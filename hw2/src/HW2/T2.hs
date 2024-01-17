module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sym arr = fromList (foldr f [[]] arr)
                        where
                          f _ [] = mempty 
                          f el list@(x : t) = if el /= sym 
                                    then (el : x) : t 
                                    else [] : list 


joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep list = foldl1 (\a b -> a ++ (sep : b)) (toList list) 

module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ s Leaf = s
tfoldr f s (Branch _ Leaf x Leaf) = f x s
tfoldr f s (Branch _ left x right) = res 
                                   where 
                                    right_f = tfoldr f s right
                                    head_f = f x right_f
                                    res = tfoldr f head_f left

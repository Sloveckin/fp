module HW2.T3
  ( epart
  , mcat
  ) where

import Data.Foldable ()

mcat :: Monoid a => [Maybe a] -> a
mcat arr = res
      where
        f ac Nothing = ac 
        f ac (Just x) = ac <> x
        res = foldl f mempty arr

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart arr = res 
      where 
        fun (Left x) = (x, mempty)
        fun (Right x) = (mempty, x)
        res = foldMap fun arr 
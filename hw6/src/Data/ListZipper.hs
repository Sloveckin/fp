-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  ( ListZipper (..)
  ,lLeft
  ,lRight
  ,lWrite
  ,toList
  , genericMove
  ) where

import Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ ls a rs) = LZ (map f ls) (f a) (map f rs)


instance Comonad ListZipper where
  extract (LZ _ a _) = a

  extend f a = f <$> duplicate a

lLeft :: ListZipper a -> ListZipper a
lLeft (LZ (l : ls) c rs) = LZ ls l (c : rs)
lLeft lz                 = lz

lRight :: ListZipper a -> ListZipper a
lRight (LZ ls c (r : rs)) = LZ (c : ls) r rs
lRight lz                 = lz

lWrite :: a -> ListZipper a -> ListZipper a
lWrite a (LZ ls _ rs) = LZ ls a rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

genericMove :: (z a -> z a)
            -> (z a -> z a)
            -> z a
            -> ListZipper (z a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)


iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f
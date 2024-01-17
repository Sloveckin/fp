module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a 

--           n             f         a 
-- (a -> a) -> a -> a -> (a -> a) -> a -> a
ns :: Nat a -> Nat a
ns n f a = f $ n f a 

nplus :: Nat a -> Nat a -> Nat a
nplus n1 n2 f a = n1 f $ n2 f a

nmult :: Nat a -> Nat a -> Nat a
nmult n1 n2 = n1 . n2


nFromNatural :: Natural -> Nat a
nFromNatural 0   = nz 
nFromNatural num = \f a -> f $ nFromNatural (num - 1) f a

-- (a -> a) -> a -> a -> a
nToNum :: Num a => Nat a -> a
nToNum nat = nat (+ 1) 0 

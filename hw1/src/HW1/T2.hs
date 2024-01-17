module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus x Z = x 
nplus x1 (S x2) = S (nplus x1 x2)

nmult :: N -> N -> N
nmult _ Z = Z 
nmult x1 (S x2) = nplus (nmult x1 x2) x1

nsub :: N -> N -> Maybe N
nsub Z Z = Just Z 
nsub x Z = Just x
nsub Z _ = Nothing
nsub (S x1) (S x2) = nsub x1 x2


ncmp :: N -> N -> Ordering
ncmp x y = case nsub x y of
                Just Z -> EQ 
                Just _ -> GT
                Nothing -> LT



nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum n = res
  where 
    createNumber Z m = m 
    createNumber (S x) m = createNumber x (m + 1)
    res = createNumber n 0


-- Advanced
nEven :: N -> Bool
nEven Z = True
nEven (S Z) = False
nEven (S (S x)) = nEven x


nOdd :: N -> Bool
nOdd = not . nEven  

ndiv :: N -> N -> N
ndiv x y = res 
      where 
        f xx yy cc = if ncmp xx yy == LT
                  then cc
                  else f (sub2 xx yy) yy (S cc)
        res = f x y Z 


sub2 :: N -> N -> N
sub2 Z (S _) = undefined 
sub2 Z Z = Z
sub2 x Z = x 
sub2 (S x1) (S x2) = sub2 x1 x2 



nmod :: N -> N -> N
nmod n m = res
        where 
          nm = ndiv n m
          res = sub2 n (nmult m nm)

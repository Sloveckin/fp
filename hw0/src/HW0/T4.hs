module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Numeric.Natural (Natural)
import Data.Function (fix)

repeat' :: a -> [a]
repeat' a = fix (a :) --fix (\rec x -> x : rec a) a 

map' :: (a -> b) -> [a] -> [b]
map' f = fix(\rec arr -> case arr of
  [] -> []
  a : as -> f a : rec as)

fib :: Natural -> Natural
fib num = fix (\rec i prev' prev -> if i == 0 then prev' else rec (i - 1) prev (prev' + prev)) num 0 1


fac :: Natural -> Natural
fac = fix (\rec x -> if x == 0 then 1 else x * rec (x - 1)) 
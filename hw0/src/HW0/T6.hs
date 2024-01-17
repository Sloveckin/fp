module HW0.T6
  ( a
  , a_whnf
  , b
  , b_whnf
  , c
  , c_whnf
  ) where

import HW0.T1 (distrib)
import Data.Char (isSpace)

a :: (Either [Char] b, Either [Char] c)
a = distrib (Left ("AB" ++ "CD" ++ "EF"))

a_whnf :: (Either [Char] b1, Either [Char] b2)
a_whnf = (Left $ "AB" ++ "CD" ++ "EF", Left $ "AB" ++ "CD" ++ "EF") -- конструктор пары

b :: [Bool]
b = map isSpace "Hello, World"

b_whnf :: [Bool]
b_whnf = isSpace 'H' : map isSpace "ello, World" -- конструктор списка 

c :: [Char]
c = if 1 > 0 || error "X" then "Y" else "Z"

c_whnf :: [Char]
c_whnf = "Y" -- нормальная форма => слабая головная нормальная форма

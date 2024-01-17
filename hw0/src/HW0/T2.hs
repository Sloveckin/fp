module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

type Not a = a -> Void

-- a -> (a -> Void) -> Void

doubleNeg :: a -> Not (Not a)
doubleNeg a f = f a   

-- Not (Not (Not a)) -> Not a 
-- Not (Not (Not a)) -> (a -> void)
-- ((Not (Not a)) -> void) -> (a -> void)
-- (((Not a) -> void) -> void) -> (a -> void)
-- (((a -> void) -> void) -> void) -> (a -> void) 
-- P.s. это довольно забавно


reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg f a = f $ doubleNeg a 

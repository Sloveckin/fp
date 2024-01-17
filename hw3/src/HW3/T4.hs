module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1
import qualified Control.Monad

newtype State s a = S { runS :: s -> Annotated s a } -- a :# s

mapState :: (a -> b) -> State s a -> State s b
mapState f (S r) = S $ mapAnnotated f . r

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState (S r) = S $ \s ->
  let S t :# e = r s
  in t e

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) = Control.Monad.ap

instance Monad (State s) where
  (>>=) state f = joinState $ fmap f state

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) x y = Op (Add x y)
  (-) x y = Op (Sub x y)
  (*) x y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  (/) x y = Op (Div x y)
  fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval (Val x)        = return x
eval (Op (Add x y)) = bin x y (+) Add
eval (Op (Mul x y)) = bin x y (*) Mul
eval (Op (Div x y)) = bin x y (/) Div 
eval (Op (Sub x y)) = bin x y (-) Sub
eval (Op (Abs x))   = un x abs Abs
eval (Op (Sgn x))   = un x signum Sgn


bin :: Expr -> Expr -> (Double -> Double -> Double) -> (Double -> Double -> Prim Double) -> State [Prim Double] Double
bin x y f g = do 
  a <- eval x 
  b <- eval y 
  modifyState (g a b : ) 
  return $ f a b


un :: Expr -> (Double -> Double) -> (Double -> Prim Double) -> State [Prim Double] Double
un x f g = do
  a <- eval x
  modifyState (g a :)
  return $ f a

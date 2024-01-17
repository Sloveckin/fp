module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import qualified Control.Monad

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES r) = ES $ mapExcept (mapAnnotated f) . r 

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success $ a :# s

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES r) = ES $ \s -> case r s of 
  Error e             -> Error e
  Success (ES g :# t) -> g t 


modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success $ () :# f s 

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e 

instance Functor (ExceptState e s) where
  fmap = mapExceptState 

instance Applicative (ExceptState e s) where
  pure = wrapExceptState 
  (<*>) = Control.Monad.ap 

instance Monad (ExceptState e s) where
  (>>=) state f = joinExceptState $ fmap f state 

data EvaluationError = DivideByZero
  deriving Show


eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x)        = return x
eval (Op (Add x y)) = bin x y (+) Add False
eval (Op (Mul x y)) = bin x y (*) Mul False
eval (Op (Div x y)) = bin x y (/) Div True 
eval (Op (Sub x y)) = bin x y (-) Sub False
eval (Op (Abs x))   = un x abs Abs
eval (Op (Sgn x))   = un x signum Sgn


bin :: Expr -> Expr -> (Double -> Double -> Double) -> (Double -> Double -> Prim Double) -> Bool -> ExceptState EvaluationError [Prim Double] Double
bin x y f g flag = do 
  a <- eval x 
  b <- eval y 
  if flag && b == 0 
  then do 
    throwExceptState DivideByZero
  else do
    modifyExceptState (g a b : ) 
    return $ f a b

un :: Expr -> (Double -> Double) -> (Double -> Prim Double) -> ExceptState EvaluationError [Prim Double] Double
un x f g = do
  a <- eval x
  modifyExceptState (g a :)
  return $ f a

mapAnnotated :: (a -> b) -> Annotated e a -> Annotated e b
mapAnnotated f (a :# e) = f a :# e

mapExcept :: (a -> b) -> Except e a -> Except e b
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success $ f a

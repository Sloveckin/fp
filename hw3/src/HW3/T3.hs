module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption (Some x) = x
joinOption None     = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)   = Error e 
joinExcept (Success a) = a 

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# e2 <> e1

joinList :: List (List a) -> List a
joinList Nil        = Nil 
joinList (a :. Nil) = a
joinList (a :. x)   = sumList a (joinList x)
    where
      sumList Nil t      = t
      sumList (h :. t) b = h :. sumList t b


joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F fun) = F $ \i -> getFromFun (fun i) i

getFromFun :: Fun i a -> (i -> a)
getFromFun (F fun) = fun

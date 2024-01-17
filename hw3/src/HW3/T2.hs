module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  , sumList
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)        = None 
distOption (_, None)        = None
distOption (Some a, Some b) = Some (a, b)

wrapOption :: a -> Option a
wrapOption = Some 

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 b1, P a2 b2) = P (a1, a2) (b1, b2)

wrapPair :: a -> Pair a
wrapPair x = P x x 

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 b1 c1 d1, Q a2 b2 c2 d2) = Q (a1, a2) (b1, b2) (c1, c2) (d1, d2)

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x 

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# e1 <> e2

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)           = Error e 
distExcept (_, Error e)           = Error e
distExcept (Success a, Success b) = Success (a, b)

wrapExcept :: a -> Except e a
wrapExcept = Success  

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, High b)     = High (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)


wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low 

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x1 :> xs1, x2 :> xs2) = (x1, x2) :> distStream (xs1, xs2)

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x 

distList :: (List a, List b) -> List (a, b)
distList (_, Nil)          = Nil
distList (Nil, _)          = Nil
distList (x1 :. xs1, list) = sumList (foo x1 list) (distList (xs1, list))  

foo :: a -> List b -> List (a, b)
foo _ Nil       = Nil
foo a (x :. xs) = (a, x) :. foo a xs


sumList :: List a -> List a -> List a
sumList Nil t      = t 
sumList (a :. t) b = a :. sumList t b


wrapList :: a -> List a
wrapList x = x :. Nil 

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F a, F b) = F $ \arg -> (a arg, b arg) 


-- We can't use const in this task  
wrapFun :: a -> Fun i a
wrapFun x = F $ \_ -> x 

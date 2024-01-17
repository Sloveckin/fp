module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last a) t = a :+ t
  (<>) (a :+ t) b = a :+ (<>) t b


data Inclusive a b = This a | That b | Both a b
  deriving Show

-- You may necessary constraints there
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This i) (This j) = This (i <> j)
  (<>) (That i) (That j) = That (i <> j)
  (<>) (This i) (That j) = Both i j
  (<>) (That j) (This i) = Both i j
  (<>) (Both i j) (This k) = Both (i <> k) j
  (<>) (Both i j) (That k) = Both i (j <> k) 
  (<>) (This k) (Both i j) = Both (k <> i) j
  (<>) (That k) (Both i j) = Both i (k <> j) 
  (<>) (Both i j) (Both k l) = Both (i <> k) (j <> l)

newtype DotString = DS String
  deriving Show

{- Не очень понятно, как здесь использовать mempty из Monoid. Писать руками (DS "") не сложно,
  но если там будет что-то сложнее? --}
instance Semigroup DotString where
  (<>) a (DS "") = a
  (<>) (DS "") a = a
  (<>) (DS a) (DS b) = DS (a ++ ('.' : b))

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F f) (F s) = F (f . s)


instance Monoid (Fun a) where
  mempty = F id

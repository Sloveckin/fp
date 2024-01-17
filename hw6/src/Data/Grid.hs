-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..)
  , neighbours
  , gWrite
  , extend
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper 
import Control.Monad (liftM2)

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Functor Grid where
  fmap f (Grid g) = 
    let hlp = fmap (fmap f) g 
    in  Grid hlp 

instance Comonad Grid where
  extract = gRead 

  duplicate = Grid . fmap horizontal . vertical 

gUp :: Grid a -> Grid a
gUp (Grid g) = Grid $ lLeft g

gDown :: Grid a -> Grid a
gDown (Grid g) = Grid $ lRight g

gLeft :: Grid a -> Grid a 
gLeft (Grid g) = Grid $ lLeft <$> g 

gRight :: Grid a -> Grid a 
gRight (Grid g) = Grid $ lRight <$> g

gRead :: Grid a -> a
gRead (Grid g) = extract $ extract g

gWrite :: a -> Grid a -> Grid a
gWrite a (Grid g) = Grid $ lWrite newLine g
  where 
    prevLine = extract g
    newLine  = lWrite a prevLine

horizontal :: Grid a -> ListZipper (Grid a)
horizontal = genericMove gLeft gRight

vertical :: Grid a -> ListZipper (Grid a)
vertical   = genericMove gUp   gDown

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [gLeft, gRight]
        verticals   = [gUp, gDown]

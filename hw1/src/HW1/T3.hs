module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)


getData :: Tree a -> (Int, Int)
getData (Branch info _ _ _) = info
getData Leaf = (0, 0)

getLeft :: Tree a -> Tree a
getLeft (Branch _ left _ _ ) = left
getLeft Leaf = undefined

getRight :: Tree a -> Tree a
getRight (Branch _ _ _ right) = right
getRight Leaf = undefined

getHead :: Tree a -> a
getHead (Branch _ _ x _) = x
getHead Leaf = undefined

tsize :: Tree a -> Int
tsize (Branch (_, size) _ _ _) = size
tsize Leaf = 0

tdepth :: Tree a -> Int
tdepth (Branch (height, _ ) _ _ _) = height
tdepth Leaf = 0

tmember :: Ord a => a -> Tree a -> Bool
tmember num (Branch _ left x right)
  | num == x = True
  | num > x = tmember num right
  | num < x = tmember num left
tmember _ Leaf = False

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert num (Branch info left x right)
  | num == x = Branch info left x right
  | num > x = preRotate (updateData left x (tinsert num right))
  | num < x = preRotate (updateData (tinsert num left) x right)
tinsert x Leaf = Branch (1, 1) Leaf x Leaf

preRotate :: Tree a -> Tree a
preRotate tree
  | (tdepth . getLeft $ tree) - (tdepth . getRight $ tree) == 2 = preR tree
  | (tdepth . getRight $ tree) - (tdepth . getLeft $ tree) == 2 = preL tree 
  | otherwise = tree


preL :: Tree a -> Tree a
preL tree 
  | (tdepth . getRight . getRight $ tree) - (tdepth . getLeft . getRight $ tree) == 1 = rotateLeft tree
  | otherwise = bigRotateLeft tree 


rotateLeft :: Tree a -> Tree a
rotateLeft tree = res 
        where 
          a = getHead tree
          l = getLeft tree
          b = getHead . getRight $ tree
          c = getLeft . getRight $ tree
          r = getRight . getRight $ tree

          left = updateData l a c
          res = updateData left b r

bigRotateLeft :: Tree a -> Tree a
bigRotateLeft (Branch _ left x right) = res 
                   where
                      r = rotateRight right
                      res = rotateLeft (updateData left x r)
bigRotateLeft Leaf = undefined


preR :: Tree a -> Tree a
preR tree
  | (tdepth . getLeft . getLeft $ tree) - (tdepth . getRight . getLeft $ tree) == 1 = rotateRight tree 
  | otherwise = bigRotateRight tree 



rotateRight :: Tree a -> Tree a
rotateRight tree = res
        where
          x = getHead tree
          c = getRight tree
          y = getHead . getLeft $ tree
          b = getRight . getLeft $ tree
          a = getLeft . getLeft $ tree

          right = updateData b x c
          res = updateData a y right
          

bigRotateRight :: Tree a -> Tree a
bigRotateRight (Branch _ left x right) = res
                    where 
                      l = rotateLeft left
                      res = rotateRight (updateData l x right)
bigRotateRight Leaf = undefined

updateData :: Tree a -> a -> Tree a -> Tree a
updateData l x r = Branch (getMaxFromData (getData l) (getData r)) l x r


getMaxFromData :: (Int, Int) -> (Int, Int) -> (Int, Int)
getMaxFromData (h1, s1) (h2, s2) = (max h1 h2 + 1, s1 + s2 + 1)


tFromList :: Ord a => [a] -> Tree a
tFromList a = res
          where
            createTree [] tree = tree
            createTree arr tree = createTree (tail arr) (tinsert (head arr) tree)
            res = createTree a Leaf

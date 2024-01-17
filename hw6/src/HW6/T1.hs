module HW6.T1
  ( BucketsArray
  , CHT (..)
  , getCapacity
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy
import Data.Array.Base (MArray(..), readArray, writeArray, getElems)
import Data.Hashable
import Control.Monad (when, forM_)


-- | Get init capacity
initCapacity :: Int
initCapacity = 16

-- | Load factor
--
-- When chtSize >= capacity * loadFactor is solved, the capacity doubles.
loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

-- | Create emtpy Concurrent Hash Table
newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ do
  size <- newTVar 0
  arr  <- newArray (0, initCapacity - 1) [] >>= newTVar -- it's the hardest part of T1
  return $ CHT arr size

-- | Get Maybe element by key from Councurrent Hash Table
getCHT :: (MonadConc m, Eq k, Hashable k)
       => k -- ^ Key 
       -> CHT (STM m) k v -- ^ Concurrent Hash Table 
       -> m (Maybe v)
getCHT key mp = atomically $ do
  cap  <- getCapacity mp
  arr  <- readTVar $ chtBuckets mp
  buck <- readArray arr (getIdx key cap)
  return $ lookup key buck

-- | Put key-value in Councurrent Hash Table
--
-- If key has been  submitted, the value is replaced.
-- if after puttting key-value chtSize >= capacity * loadFactor is solved, the capacity doubles and the indexes for each key-value are changed.
putCHT :: (MonadConc m, Hashable k, Eq k)
       => k -- ^ Key
       -> v -- ^ Value
       -> CHT (STM m) k v -- ^ Councurrent Hash Table
       -> m ()
putCHT key value mp = atomically $ do
  cap  <- getCapacity mp
  size <- addElement (key, value) cap mp
  when (needResize size cap) (resize cap mp)

-- | Get capacity
getCapacity :: MonadSTM m
            => CHT m k v -- ^ Councurrent Hash Table
            -> m Int
getCapacity mp = readTVar (chtBuckets mp) >>= getNumElements

-- | Check if it need to double capacity.
needResize :: Int -- ^ Size
           -> Int -- ^ Capacity
           -> Bool
needResize size cap = fromIntegral size >= fromIntegral cap * loadFactor

-- | Resize array of bucktes with changing indexed for each key-value.
resize :: (MonadSTM m, Hashable k)
       => Int -- ^ New capacity 
       -> CHT m k v -- ^ Councurrent Hash Table 
       -> m ()
resize cap mp = do
   arr    <- readTVar $ chtBuckets mp
   list   <- getElems arr
   newArr <- newArray (0, 2 * cap - 1) []
   writeTVar (chtBuckets mp) newArr
   forM_ (concat list) (rehashPair newArr)

-- | Changing index for each key-value
rehashPair :: (MonadSTM m, Hashable k)
         => BucketsArray m k v -- ^ New array of buckets 
         -> (k, v) -- ^ Pair that we need recalculate index
         -> m ()
rehashPair arr pair = do
    cap <- getNumElements arr
    let idx = getIdx (fst pair) cap
    buck <- readArray arr idx
    writeArray arr idx (pair : buck)

-- | Push new key-value in map
--
-- Or replace the value of a key that was already there.
addElement :: (MonadSTM m, Eq k, Hashable k)
           => (k, v) -- ^ Key-Value
           -> Int -- ^ Capacity
           -> CHT m k v -- ^ Councurrent Hash Table
           -> m Int
addElement pair cap mp = do
    let index = getIdx key cap
    oldSize <- readTVar $ chtSize mp
    arr     <- readTVar $ chtBuckets mp
    oldBuck <- readArray arr index
    let newBuck = filter skip oldBuck

    -- If length oldBcuk == length newBuck => key is new
    when (length oldBuck == length newBuck) $ writeTVar (chtSize mp) (oldSize + 1)

    writeArray arr index (pair : newBuck)
    readTVar $ chtSize mp
      where
        key = fst pair
        skip (k, _) = k /= key

-- | Get count of element in Concurrent Hash Table
--
-- Not to be confused with capacity.
sizeCHT :: MonadConc m
        => CHT (STM m) k v -- ^ Concurrent Hash Table
        -> m Int
sizeCHT = readTVarConc . chtSize

-- | Get index of bucket by hash (key) % capacity
getIdx :: Hashable k
       => k  -- ^ Key
       -> Int -- ^ Capacity 
       -> Int
getIdx key cap = hash key `mod` cap


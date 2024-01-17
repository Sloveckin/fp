{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module HW6.T2
  ( TSet
  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

type TSet = [Symbol]


type family Contains (name :: Symbol) (set :: TSet) :: Bool where
            Contains _ '[]              = 'False 
            Contains name (name ': set) = 'True
            Contains name (x ': set)    = Contains name set


type family Delete (name :: Symbol) (set :: TSet) :: TSet where
            Delete _ '[]              = '[]
            Delete name (name ': set) = Delete name set
            Delete name (x  ': set)   = x ': Delete name set

type family Add (v :: Symbol) (set :: TSet) :: TSet where
            Add v '[]        = '[v]
            Add v (v ': set) = v ': set 
            Add v (x ': set) = x ': Add v set

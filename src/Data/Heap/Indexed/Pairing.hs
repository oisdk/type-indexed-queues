{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Data.Heap.Indexed.Pairing
  (Pairing(..))
  where

import           Data.Heap.Indexed.Class
import           GHC.TypeLits

data Pairing n a where
  E :: Pairing 0 a
  T :: a -> HVec n a -> Pairing (1 + n) a

type role Pairing nominal nominal

data HVec n a where
  HNil :: HVec 0 a
  HCons :: Pairing m a -> HVec n a -> HVec (m + n) a

instance IndexedPriorityQueue Pairing where
    minView (T x hs) = (x, mergePairs hs)
    {-# INLINABLE minView #-}
    singleton a = T a HNil
    empty = E
    insert = merge . singleton
    {-# INLINABLE insert #-}

instance MeldableIndexedPriorityQueue Pairing where
    merge E ys = ys
    merge xs E = xs
    merge h1@(T x xs) h2@(T y ys)
      | x <= y = T x (HCons h2 xs)
      | otherwise = T y (HCons h1 ys)
    {-# INLINABLE merge #-}

mergePairs :: Ord a => HVec n a -> Pairing n a
mergePairs HNil = E
mergePairs (HCons h HNil) = h
mergePairs (HCons h1 (HCons h2 hs)) =
    merge (merge h1 h2) (mergePairs hs)
{-# INLINABLE mergePairs #-}

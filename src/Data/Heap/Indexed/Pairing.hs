{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Data.Heap.Indexed.Pairing
  (Heap
  ,empty
  ,singleton
  ,insert
  ,merge
  ,minView)
  where

import           GHC.TypeLits

data Heap n a where
  E :: Heap 0 a
  T :: a -> HVec n a -> Heap (1 + n) a

type role Heap nominal nominal

data HVec n a where
  HNil :: HVec 0 a
  HCons :: Heap m a -> HVec n a -> HVec (m + n) a

empty :: Heap 0 a
empty = E

singleton :: a -> Heap 1 a
singleton a = T a HNil

insert :: Ord a => a -> Heap n a -> Heap (1 + n) a
insert = merge . singleton
{-# INLINABLE insert #-}

merge :: Ord a => Heap m a -> Heap n a -> Heap (m + n) a
merge E ys = ys
merge xs E = xs
merge h1@(T x xs) h2@(T y ys)
  | x <= y = T x (HCons h2 xs)
  | otherwise = T y (HCons h1 ys)
{-# INLINABLE merge #-}

minView :: Ord a => Heap (1 + n) a -> (a, Heap n a)
minView (T x hs) = (x, mergePairs hs)
{-# INLINABLE minView #-}

mergePairs :: Ord a => HVec n a -> Heap n a
mergePairs HNil = E
mergePairs (HCons h HNil) = h
mergePairs (HCons h1 (HCons h2 hs)) =
    merge (merge h1 h2) (mergePairs hs)
{-# INLINABLE mergePairs #-}

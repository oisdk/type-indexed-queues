module Data.Heap.Pairing
  (Pairing
  ,singleton
  ,insert
  ,minView)
  where

import           Data.Heap.Class

data Pairing a = E | T a [Pairing a]

instance Ord a => Monoid (Pairing a) where
    mempty = E
    mappend E ys = ys
    mappend xs E = xs
    mappend h1@(T x xs) h2@(T y ys)
      | x <= y = T x (h2 : xs)
      | otherwise = T y (h1 : ys)
    {-# INLINABLE mappend #-}

instance MinHeap Pairing where
    singleton a = T a []
    insert = mappend . singleton
    {-# INLINABLE insert #-}
    minView (T x hs) = Just (x, mergePairs hs)
    minView E        = Nothing
    {-# INLINABLE minView #-}
    merge = mappend
    {-# INLINE merge #-}
    empty = mempty
    {-# INLINE empty #-}

mergePairs :: Ord a => [Pairing a] -> Pairing a
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1 : h2 : hs) =
    mappend (mappend h1 h2) (mergePairs hs)
{-# INLINABLE mergePairs #-}

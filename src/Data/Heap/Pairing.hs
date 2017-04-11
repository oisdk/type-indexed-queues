{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Heap.Pairing
  (Heap
  ,singleton
  ,insert
  ,minView)
  where

import           Data.Heap.Class

data Heap a = E | T a (HVec a)

type HVec a = [Heap a]

instance Ord a => Monoid (Heap a) where
    mempty = E
    mappend E ys = ys
    mappend xs E = xs
    mappend h1@(T x xs) h2@(T y ys)
      | x <= y = T x (h2 : xs)
      | otherwise = T y (h1 : ys)
    {-# INLINABLE mappend #-}

instance Ord a => MinHeap Heap a where
    singleton a = T a []
    insert = mappend . singleton
    {-# INLINABLE insert #-}
    minView (T x hs) = Just (x, mergePairs hs)
    minView E        = Nothing
    {-# INLINABLE minView #-}

mergePairs :: Ord a => HVec a -> Heap a
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1 : h2 : hs) =
    mappend (mappend h1 h2) (mergePairs hs)
{-# INLINABLE mergePairs #-}

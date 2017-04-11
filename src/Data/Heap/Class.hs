{-# LANGUAGE ScopedTypeVariables #-}

module Data.Heap.Class where

import Data.List (unfoldr)

class MinHeap h  where
    {-# MINIMAL minView , singleton , merge , empty #-}
    minView
        :: Ord a
        => h a -> Maybe (a, h a)
    singleton
        :: Ord a
        => a -> h a
    insert
        :: Ord a
        => a -> h a -> h a
    insert = merge . singleton
    merge
        :: Ord a
        => h a -> h a -> h a
    empty
        :: Ord a
        => h a

newtype HeapMonoid h a = HeapMonoid { runHeapMonoid :: h a }

instance (MinHeap h, Ord a) => Monoid (HeapMonoid h a) where
    mappend (HeapMonoid xs) (HeapMonoid ys) = HeapMonoid (merge xs ys)
    mempty = HeapMonoid empty

toList :: (Ord a, MinHeap h) => h a -> [a]
toList = unfoldr minView

fromList :: (Ord a, MinHeap h) => [a] -> h a
fromList = foldr insert empty

fromFoldable :: (MinHeap h, Foldable f, Ord a) => f a -> h a
fromFoldable = runHeapMonoid . foldMap (HeapMonoid . singleton)

heapSort :: (MinHeap h, Ord a) => p h -> [a] -> [a]
heapSort (_ :: p h) = toList . (fromList :: Ord a => [a] -> h a)

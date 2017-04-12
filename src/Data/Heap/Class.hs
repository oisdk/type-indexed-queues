{-# LANGUAGE ScopedTypeVariables #-}

module Data.Heap.Class where

import Data.List (unfoldr)

class PriorityQueue h  where

    {-# MINIMAL minView , insert , empty #-}

    minView
        :: Ord a
        => h a -> Maybe (a, h a)

    insert
        :: Ord a
        => a -> h a -> h a
    empty
        :: Ord a
        => h a
    singleton
        :: Ord a
        => a -> h a
    singleton = flip insert empty

    toList :: Ord a => h a -> [a]
    toList = unfoldr minView

    fromList :: Ord a => [a] -> h a
    fromList = foldr insert empty

    heapSort :: Ord a => p h -> [a] -> [a]
    heapSort (_ :: p h) = toList . (fromList :: Ord a => [a] -> h a)

class PriorityQueue h => MeldableQueue h where
    merge :: Ord a => h a -> h a -> h a

    fromFoldable :: (Foldable f, Ord a) => f a -> h a
    fromFoldable = runQueueWrapper . foldMap (QueueWrapper . singleton)

newtype QueueWrapper h a = QueueWrapper { runQueueWrapper :: h a }

instance (MeldableQueue h, Ord a) => Monoid (QueueWrapper h a) where
    mappend (QueueWrapper xs) (QueueWrapper ys) = QueueWrapper (merge xs ys)
    mempty = QueueWrapper empty

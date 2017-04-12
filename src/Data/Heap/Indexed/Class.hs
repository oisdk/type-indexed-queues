{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Heap.Indexed.Class where

import           GHC.TypeLits

class IndexedPriorityQueue h  where

    {-# MINIMAL minView, insert, empty #-}

    empty
        :: Ord a
        => h 0 a

    minView
        :: Ord a
        => h (1 + n) a -> (a, h n a)

    singleton
        :: Ord a
        => a -> h 1 a
    singleton = flip insert empty

    insert
        :: Ord a
        => a -> h n a -> h (1 + n) a

class IndexedPriorityQueue h =>
      MeldableIndexedPriorityQueue h  where
    merge
        :: Ord a
        => h n a -> h m a -> h (n + m) a

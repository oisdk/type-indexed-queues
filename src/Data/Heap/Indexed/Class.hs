{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Heap.Indexed.Class where

import           GHC.TypeLits

class IndexedHeap h where
    empty :: Ord a => h 0 a
    merge :: Ord a => h n a -> h m a -> h (n + m) a
    minView :: Ord a => h (1 + n) a -> (a, h n a)
    singleton :: Ord a => a -> h 1 a

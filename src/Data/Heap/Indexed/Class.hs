{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.Heap.Indexed.Class where

import           GHC.TypeLits

class IndexedPriorityQueue h  where

    {-# MINIMAL insert, empty, minViewMay, minView #-}

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

    minViewMay
       :: Ord a
       => h n a
       -> (n ~ 0 => b)
       -> (forall m. (1 + m) ~ n => a -> h m a -> b)
       -> b

class IndexedPriorityQueue h =>
      MeldableIndexedQueue h  where
    merge
        :: Ord a
        => h n a -> h m a -> h (n + m) a

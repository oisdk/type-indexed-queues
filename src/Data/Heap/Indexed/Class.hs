{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.Heap.Indexed.Class where

import           GHC.TypeLits

class IndexedPriorityQueue h a where

    {-# MINIMAL insert, empty, minViewMay, minView #-}

    empty
        :: h 0 a

    minView
        :: h (1 + n) a -> (a, h n a)

    singleton
        :: a -> h 1 a
    singleton = flip insert empty

    insert
        :: a -> h n a -> h (1 + n) a

    minViewMay
       :: h n a
       -> (n ~ 0 => b)
       -> (forall m. (1 + m) ~ n => a -> h m a -> b)
       -> b

class IndexedPriorityQueue h a =>
      MeldableIndexedQueue h a where
    merge
        :: h n a -> h m a -> h (n + m) a

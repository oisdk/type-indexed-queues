{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Classes for common functions between the heaps.
module Data.Heap.Indexed.Class where

import           GHC.TypeLits

-- | A classed for indexed priority queues. Equivalent to 'Data.Heap.Queue'
-- except the queues are indexed by their sizes.
class IndexedQueue h a where

    {-# MINIMAL insert, empty, minViewMay, minView #-}

    -- | The empty queue
    empty
        :: h 0 a

    -- | Return the minimal element, and the rest of the queue.
    minView
        :: h (1 + n) a -> (a, h n a)

    -- | A queue with one element.
    singleton
        :: a -> h 1 a
    singleton = flip insert empty

    -- | Add an element to the queue.
    insert
        :: a -> h n a -> h (1 + n) a

    -- | Pattern match on the queue, and provide a proof that it
    -- is/isn't empty to the caller.
    minViewMay
       :: h n a
       -> (n ~ 0 => b)
       -> (forall m. (1 + m) ~ n => a -> h m a -> b)
       -> b

-- | Queues which can be merged. Conforming members should
-- form a monoid under 'merge' and 'empty'.
class IndexedQueue h a =>
      MeldableIndexedQueue h a where
    -- | Merge two heaps. This operation is associative, and has the
    -- identity of 'empty'.
    --
    -- @'merge' x ('merge' y z) = 'merge' ('merge' x y) z@
    --
    -- @'merge' x 'empty' = 'merge' 'empty' x = x@
    merge
        :: h n a -> h m a -> h (n + m) a

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Classes for the various heaps, mainly to avoid name clashing.
module Data.Queue.Class
  (Queue(..)
  ,MeldableQueue(..)
  ,showsPrecQueue
  ,readPrecQueue
  ,eqQueue
  ,cmpQueue)
  where

import           Data.List (unfoldr)
import           Data.Function (on)
import           Data.Coerce (Coercible,coerce)

import           Data.Set (Set)
import qualified Data.Set as Set

-- | A class for queues. Conforming members can have their own
-- definition of order on their contents. (i.e., 'Ord' is not required)
class Queue h a where

    {-# MINIMAL minView , insert , empty #-}

    -- | Return the first element, and the remaining elements,
    -- or 'Nothing' if the queue is empty. For most queues,
    -- this will be the minimal element
    minView
        :: h a -> Maybe (a, h a)

    -- | Insert an element into the queue.
    insert
        :: a -> h a -> h a

    -- | The empty queue.
    empty
        :: h a

    -- | A queue with one element.
    singleton
        :: a -> h a
    singleton = flip insert empty

    -- | Return a list of the contents of the queue, in order, from
    -- smallest to largest.
    toList :: h a -> [a]
    toList = unfoldr minView

    -- | Create a heap from a list.
    fromList :: [a] -> h a
    fromList = foldr insert empty

    -- | Perform heap sort on a list of items.
    heapSort :: p h -> [a] -> [a]
    heapSort (_ :: p h) = toList . (fromList :: [a] -> h a)

-- | A class for meldable queues. Conforming members should
-- form a monoid under 'merge' and 'empty'.
class Queue h a => MeldableQueue h a where

    {-# MINIMAL merge #-}
    -- | Merge two heaps. This operation is associative, and has the
    -- identity of 'empty'.
    --
    -- @'merge' x ('merge' y z) = 'merge' ('merge' x y) z@
    --
    -- @'merge' x 'empty' = 'merge' 'empty' x = x@
    merge :: h a -> h a -> h a

    -- | Create a heap from a 'Foldable' container. This operation is
    -- provided to allow the use of 'foldMap', which may be
    -- asymptotically more efficient. The default definition uses
    -- 'foldMap'.
    fromFoldable :: (Foldable f) => f a -> h a
    fromFoldable = runQueueWrapper #. foldMap (QueueWrapper #. singleton)

newtype QueueWrapper h a = QueueWrapper
    { runQueueWrapper :: h a
    }

instance MeldableQueue h a =>
         Monoid (QueueWrapper h a) where
    mempty = QueueWrapper empty
    mappend =
        (coerce :: (h a -> h a -> h a) -> QueueWrapper h a -> QueueWrapper h a -> QueueWrapper h a)
            merge
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

-- | A default definition for 'showsPrec'.
showsPrecQueue :: (Queue h a, Show a) => Int -> h a -> ShowS
showsPrecQueue d xs =
    showParen (d >= 11) (showString "fromList " . showList (toList xs))

-- | A default definition for 'readsPrec'.
readPrecQueue
  :: (Read a, Queue h a) => Int -> ReadS (h a)
readPrecQueue d =
    readParen
        (d > 10)
        (\xs ->
              [ (fromList x, zs)
              | ("fromList",ys) <- lex xs
              , (x,zs) <- readList ys ])

-- | A default definition of '=='.
eqQueue :: (Eq a, Queue h a) => h a -> h a -> Bool
eqQueue = (==) `on` toList

-- | A default definition of 'compare'.
cmpQueue :: (Ord a, Queue h a) => h a -> h a -> Ordering
cmpQueue = compare `on` toList

infixr 9 #.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce

instance Ord a => Queue Set a where
    insert = Set.insert
    empty = Set.empty
    fromList = Set.fromList
    singleton = Set.singleton
    minView = Set.minView
    toList = Set.toList

instance Ord a => MeldableQueue Set a where
    merge = Set.union

instance Queue [] a where
    insert = (:)
    empty = []
    fromList = id
    singleton = (:[])
    minView [] = Nothing
    minView (x:xs) = Just (x,xs)
    toList = id

instance MeldableQueue [] a where
    merge = (++)
    fromFoldable = foldr (:) []

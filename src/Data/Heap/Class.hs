{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Data.Heap.Class
  (PriorityQueue(..)
  ,MeldableQueue(..))
  where

import           Data.List (unfoldr)

class PriorityQueue h a where

    {-# MINIMAL minView , insert , empty #-}

    minView
        :: h a -> Maybe (a, h a)

    insert
        :: a -> h a -> h a
    empty
        :: h a
    singleton
        :: a -> h a
    singleton = flip insert empty

    toList :: h a -> [a]
    toList = unfoldr minView

    fromList :: [a] -> h a
    fromList = foldr insert empty

    heapSort :: p h -> [a] -> [a]
    heapSort (_ :: p h) = toList . (fromList :: [a] -> h a)

class PriorityQueue h a => MeldableQueue h a where

    {-# MINIMAL merge #-}
    merge :: h a -> h a -> h a

    fromFoldable :: (Foldable f) => f a -> h a
    fromFoldable = runQueueWrapper . foldMap (QueueWrapper . singleton)

newtype QueueWrapper h a = QueueWrapper
    { runQueueWrapper :: h a
    }

instance MeldableQueue h a =>
         Monoid (QueueWrapper h a) where
    mempty = QueueWrapper empty
    mappend (QueueWrapper xs) (QueueWrapper ys) = QueueWrapper (merge xs ys)

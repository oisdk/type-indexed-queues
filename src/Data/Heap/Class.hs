{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Data.Heap.Class
  (PriorityQueue(..)
  ,MeldableQueue(..)
  ,showsPrecQueue
  ,readPrecQueue
  ,eqQueue
  ,cmpQueue)
  where

import           Data.List (unfoldr)
import           Data.Function (on)

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

showsPrecQueue :: (PriorityQueue h a, Show a) => Int -> h a -> ShowS
showsPrecQueue d xs =
    showParen (d >= 11) (showString "fromList " . showList (toList xs))

readPrecQueue
  :: (Read a, PriorityQueue h a) => Int -> ReadS (h a)
readPrecQueue d =
    readParen
        (d > 10)
        (\xs ->
              [ (fromList x, zs)
              | ("fromList",ys) <- lex xs
              , (x,zs) <- readList ys ])

eqQueue :: (Eq a, PriorityQueue h a) => h a -> h a -> Bool
eqQueue = (==) `on` toList

cmpQueue :: (Ord a, PriorityQueue h a) => h a -> h a -> Ordering
cmpQueue = compare `on` toList

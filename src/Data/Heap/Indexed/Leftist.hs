{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- | Statically verified, type-indexed, weight-biased leftist heaps.
module Data.Heap.Indexed.Leftist
  (Leftist(..))
  where

import           Data.Heap.Indexed.Class
import           GHC.TypeLits

import           TypeLevel.Singletons

data Leftist n a where
        Empty :: Leftist 0 a
        Node :: (m <= n)
             => !(The Nat (n + m + 1))
             -> a
             -> Leftist n a
             -> Leftist m a
             -> Leftist (n + m + 1) a

rank :: Leftist n s -> The Nat n
rank Empty          = sing
rank (Node r _ _ _) = r
{-# INLINE rank #-}

instance Ord a => IndexedPriorityQueue Leftist a where

    minView (Node _ x l r) = (x, merge l r)
    {-# INLINE minView #-}

    singleton x = Node sing x Empty Empty
    {-# INLINE singleton #-}

    empty = Empty
    {-# INLINE empty #-}

    insert = merge . singleton
    {-# INLINE insert #-}

    minViewMay Empty b _          = b
    minViewMay (Node _ x l r) _ f = f x (merge l r)

instance Ord a =>
         MeldableIndexedQueue Leftist a where
    merge Empty h2 = h2
    merge h1 Empty = h1
    merge h1@(Node w1 p1 l1 r1) h2@(Node w2 p2 l2 r2)
      | p1 < p2 =
          case ll <=. lr of
              Truey -> Node (w1 +. w2) p1 l1 (merge r1 h2)
              Falsy -> totalOrder ll lr $ Node (w1 +. w2) p1 (merge r1 h2) l1
      | otherwise =
          case rl <=. rr of
              Truey -> Node (w1 +. w2) p2 l2 (merge r2 h1)
              Falsy -> totalOrder rl rr $ Node (w1 +. w2) p2 (merge r2 h1) l2
      where
        ll = rank r1 +. w2
        lr = rank l1
        rl = rank r2 +. w1
        rr = rank l2

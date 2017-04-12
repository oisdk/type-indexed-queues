{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Data.Heap.Leftist where

import Data.Heap.Class

data Leftist a
    = Empty
    | Node {-# UNPACK #-} !Int
           a
           (Leftist a)
           (Leftist a)

rank :: Leftist s -> Int
rank Empty          = 0
rank (Node r _ _ _) = r
{-# INLINE rank #-}

instance Ord a => PriorityQueue Leftist a where

    minView Empty          = Nothing
    minView (Node _ x l r) = Just (x, merge l r)
    {-# INLINE minView #-}

    singleton x = Node 0 x Empty Empty
    {-# INLINE singleton #-}

    empty = Empty
    {-# INLINE empty #-}

    insert = merge . singleton
    {-# INLINE insert #-}

instance Ord a => MeldableQueue Leftist a where
    merge Empty h2 = h2
    merge h1 Empty = h1
    merge t1@(Node _ x1 l1 r1) t2@(Node _ x2 l2 r2)
      | x1 <= x2 = join l1 x1 (merge r1 t2)
      | otherwise = join l2 x2 (merge t1 r2)

join :: Ord a => Leftist a -> a -> Leftist a -> Leftist a
join t1 x t2
  | rank t1 >= rank t2 = Node (rank t2 + 1) x t1 t2
  | otherwise = Node (rank t1 + 1) x t2 t1
{-# INLINE join #-}

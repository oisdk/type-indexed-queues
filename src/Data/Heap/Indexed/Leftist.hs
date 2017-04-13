{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Data.Heap.Indexed.Leftist
  (Leftist(..))
  where

import           Data.Heap.Indexed.Class
import           GHC.TypeLits

data Leftist n a where
        Empty :: Leftist 0 a
        Node :: {-# UNPACK #-} !Int
             -> a
             -> Leftist n a
             -> Leftist m a
             -> Leftist (n + m + 1) a

rank :: Leftist n s -> Int
rank Empty          = 0
rank (Node r _ _ _) = r
{-# INLINE rank #-}

instance Ord a => IndexedPriorityQueue Leftist a where

    minView (Node _ x l r) = (x, merge l r)
    {-# INLINE minView #-}

    singleton x = Node 1 x Empty Empty
    {-# INLINE singleton #-}

    empty = Empty
    {-# INLINE empty #-}

    insert = merge . singleton
    {-# INLINE insert #-}

    minViewMay Empty b _          = b
    minViewMay (Node _ x l r) _ f = f x (merge l r)

instance Ord a => MeldableIndexedQueue Leftist a where
    merge Empty h2 = h2
    merge h1 Empty = h1
    merge t1@(Node _ x1 l1 r1) t2@(Node _ x2 l2 r2)
      | x1 <= x2 = join l1 x1 (merge r1 t2)
      | otherwise = join l2 x2 (merge t1 r2)

join
    :: Ord a
    => Leftist n a -> a -> Leftist m a -> Leftist (n + m + 1) a
join t1 x t2
  | rank t1 >= rank t2 = Node (rank t2 + 1) x t1 t2
  | otherwise = Node (rank t1 + 1) x t2 t1
{-# INLINE join #-}

module Data.Heap.Leftist where

import Data.Heap.Class

data Leftist a = Empty | Node a Int (Leftist a) (Leftist a)

rank :: Leftist s -> Int
rank Empty          = 0
rank (Node _ r _ _) = r

instance MinHeap Leftist where
  merge Empty h2 = h2
  merge h1 Empty = h1
  merge h1@(Node p1 w1 l1 r1) h2@(Node p2 w2 l2 r2)
    | p1 < p2 =
        if rank l1 >= rank r1 + w2
            then Node p1 (w1 + w2) l1 (merge r1 h2)
            else Node p1 (w1 + w2) (merge r1 h2) l1
    | rank l2 >= rank r2 + w1 = Node p2 (w1 + w2) l2 (merge r2 h1)
    | otherwise = Node p2 (w1 + w2) (merge r2 h1) l2

  minView Empty          = Nothing
  minView (Node x _ l r) = Just (x, merge l r)

  singleton x = Node x 0 Empty Empty
  empty = Empty

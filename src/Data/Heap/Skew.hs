module Data.Heap.Skew where

import           Data.Heap.Class

data Skew a = Empty | Node a (Skew a) (Skew a)

instance Ord a => Monoid (Skew a) where
    mempty = Empty
    mappend Empty ys = ys
    mappend xs Empty = xs
    mappend h1@(Node x lx rx) h2@(Node y ly ry)
      | x <= y    = Node x (mappend h2 rx) lx
      | otherwise = Node y (mappend h1 ry) ly

instance MinHeap Skew where
    singleton x = Node x Empty Empty
    minView Empty        = Nothing
    minView (Node x l r) = Just (x, mappend l r)
    empty = mempty
    merge = mappend

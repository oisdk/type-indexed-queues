{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Heap.Skew where

import           Data.Heap.Class

data Heap a = Empty | Node a (Heap a) (Heap a)

instance Ord a => Monoid (Heap a) where
    mempty = Empty
    mappend Empty ys = ys
    mappend xs Empty = xs
    mappend h1@(Node x lx rx) h2@(Node y ly ry)
      | x <= y    = Node x (mappend h2 rx) lx
      | otherwise = Node y (mappend h1 ry) ly

instance Ord a => MinHeap Heap a where
    singleton x = Node x Empty Empty
    minView Empty        = Nothing
    minView (Node x l r) = Just (x, mappend l r)

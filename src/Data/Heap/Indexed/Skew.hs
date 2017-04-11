{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Data.Heap.Indexed.Skew where

import           Data.Heap.Indexed.Class

import           GHC.TypeLits

data Heap n a where
        Empty :: Heap 0 a
        Node :: a -> Heap n a -> Heap m a -> Heap (1 + n + m) a

instance Ord a =>
         IndexedHeap Heap a where
    empty = Empty
    merge Empty ys = ys
    merge xs Empty = xs
    merge h1@(Node x lx rx) h2@(Node y ly ry)
      | x <= y = Node x (merge h2 rx) lx
      | otherwise = Node y (merge h1 ry) ly
    singleton x = Node x Empty Empty
    minView (Node x l r) = (x, merge l r)

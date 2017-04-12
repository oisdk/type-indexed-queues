{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Data.Heap.Indexed.Skew where

import           Data.Heap.Indexed.Class

import           GHC.TypeLits

data Skew n a where
        Empty :: Skew 0 a
        Node :: a -> Skew n a -> Skew m a -> Skew (1 + n + m) a

instance IndexedPriorityQueue Skew where
    empty = Empty
    singleton x = Node x Empty Empty
    minView (Node x l r) = (x, merge l r)
    insert = merge . singleton

instance MeldableIndexedPriorityQueue Skew where
    merge Empty ys = ys
    merge xs Empty = xs
    merge h1@(Node x lx rx) h2@(Node y ly ry)
      | x <= y = Node x (merge h2 rx) lx
      | otherwise = Node y (merge h1 ry) ly

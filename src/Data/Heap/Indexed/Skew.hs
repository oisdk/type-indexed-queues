{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Data.Heap.Indexed.Skew where

import           Data.Heap.Indexed.Class

import           GHC.TypeLits

data Skew n a where
        Empty :: Skew 0 a
        Node :: a -> Skew n a -> Skew m a -> Skew (1 + n + m) a

instance Ord a => IndexedPriorityQueue Skew a where
    empty = Empty
    singleton x = Node x Empty Empty
    minView (Node x l r) = (x, merge l r)
    insert = merge . singleton
    minViewMay Empty b _        = b
    minViewMay (Node x l r) _ f = f x (merge l r)

instance Ord a => MeldableIndexedQueue Skew a where
    merge Empty ys = ys
    merge xs Empty = xs
    merge h1@(Node x lx rx) h2@(Node y ly ry)
      | x <= y = Node x (merge h2 rx) lx
      | otherwise = Node y (merge h1 ry) ly

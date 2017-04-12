{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Heap.Skew
  (Skew(..))
  where

import           Data.BinaryTree
import           Data.Heap.Class

newtype Skew a = Skew
    { runSkew :: Tree a
    }

instance Ord a => Monoid (Skew a) where
    mempty = Skew Leaf
    mappend (Skew xs) (Skew ys) = Skew (smerge xs ys)

smerge :: Ord a => Tree a -> Tree a -> Tree a
smerge Leaf ys = ys
smerge xs Leaf = xs
smerge h1@(Node x lx rx) h2@(Node y ly ry)
  | x <= y    = Node x (smerge h2 rx) lx
  | otherwise = Node y (smerge h1 ry) ly

instance Ord a => PriorityQueue Skew a where
    singleton x = Skew (Node x Leaf Leaf)
    minView (Skew Leaf)         = Nothing
    minView (Skew (Node x l r)) = Just (x, Skew (smerge l r))
    empty = mempty
    insert = merge . singleton

instance Ord a => MeldableQueue Skew a where
    merge = mappend

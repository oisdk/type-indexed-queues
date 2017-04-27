{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor         #-}

module Data.Queue.Splay where

import           Data.BinaryTree
import           Data.Queue.Class

import           Control.DeepSeq (NFData(rnf))

-- | A simple splay heap. Based on <https://hackage.haskell.org/package/EdisonCore-1.3.1.1/docs/src/Data-Edison-Coll-SplayHeap.html#Heap this>.
newtype Splay a = Splay { runSplay :: Tree a } deriving Functor

instance Ord a => Queue Splay a where
    minView (Splay Leaf) = Nothing
    minView (Splay (Node xx aa bb)) = Just (minv aa xx bb) where
      minv Leaf x b = (x, Splay b)
      minv (Node x Leaf b) y c = (x, Splay (Node y b c))
      minv (Node y (Node a x b) c) z d = (w, Splay (Node y ab (Node z c d)))
        where
          (w,Splay ab) = minv x a b
    empty = Splay Leaf
    insert x (Splay xs) = Splay (Node x a b) where
      (a,b) = partitionLeGt x xs

instance Ord a => MeldableQueue Splay a where
    merge (Splay xx) (Splay yy) = Splay (go xx yy) where
      go Leaf ys = ys
      go (Node x a b) ys = Node x (go c a) (go d b)
        where (c,d) = partitionLeGt x ys

partitionLeGt :: Ord a => a -> Tree a -> (Tree a, Tree a)
partitionLeGt _ Leaf = (Leaf, Leaf)
partitionLeGt k t@(Node x a b) =
  if x > k then
    case a of
      Leaf -> (Leaf,t)
      Node y aa ab ->
        if y > k then
          let (small,big) = partitionLeGt k aa
          in (small, Node y big (Node x ab b))
        else
          let (small,big) = partitionLeGt k ab
          in (Node y aa small, Node x big b)
  else
    case b of
      Leaf -> (t,Leaf)
      Node y ba bb ->
        if y > k then
          let (small,big) = partitionLeGt k ba
          in (Node x a small, Node y big bb)
        else
          let (small,big) = partitionLeGt k bb
          in (Node y (Node x a ba) small, big)

instance Ord a => Monoid (Splay a) where
    mempty = empty
    mappend = merge

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance NFData a =>
         NFData (Splay a) where
    rnf (Splay x) = rnf x

instance Ord a => Eq (Splay a) where
    (==) = eqQueue

instance Ord a => Ord (Splay a) where
    compare = cmpQueue

instance (Show a, Ord a) => Show (Splay a) where
    showsPrec = showsPrecQueue

instance (Read a, Ord a) => Read (Splay a) where
    readsPrec = readPrecQueue

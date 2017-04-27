{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- | Size-indexed splay heaps.
module Data.Queue.Indexed.Splay
  (Splay(..))
  where

import           Data.Queue.Indexed.Class

import           GHC.TypeLits

import           Control.DeepSeq (NFData(rnf))

-- | A size-indexed splay heap. Based on
-- <https://hackage.haskell.org/package/EdisonCore-1.3.1.1/docs/src/Data-Edison-Coll-SplayHeap.html#Heap this>.
data Splay n a where
        Leaf :: Splay 0 a
        Node :: a -> !(Splay n a) -> !(Splay m a) -> Splay (1 + n + m) a

instance Ord a => IndexedQueue Splay a where
    minView (Node xx aa bb) = minv aa xx bb where
      minv :: Splay n a -> a -> Splay m a -> (a, Splay (n+m) a)
      minv Leaf x b = (x, b)
      minv (Node x Leaf b) y c = (x, Node y b c)
      minv (Node y (Node a x b) c) z d = (w, Node y ab (Node z c d))
        where
          (w,ab) = minv x a b
      {-# INLINE minv #-}
    {-# INLINE minView #-}
    empty = Leaf
    insert x xs = case partitionLeGt x xs of
      SumsTo a b -> Node x a b
    {-# INLINE insert #-}
    minViewMay Leaf b _ = b
    minViewMay n@Node {} _ f = uncurry f (minView n)

instance Ord a => MeldableIndexedQueue Splay a where
    merge Leaf ys = ys
    merge xs Leaf = xs
    merge (Node x a b) ys = case partitionLeGt x ys of
      SumsTo c d -> Node x (merge c a) (merge d b)
    {-# INLINE merge #-}

instance NFData a =>
         NFData (Splay n a) where
    rnf Leaf = ()
    rnf (Node x l r) = rnf x `seq` rnf l `seq` rnf r

data SumsTo n a where
    SumsTo :: !(Splay x a) -> !(Splay y a) -> SumsTo (x + y) a

partitionLeGt :: Ord a => a -> Splay n a -> SumsTo n a
partitionLeGt _ Leaf = SumsTo Leaf Leaf
partitionLeGt k t@(Node x a b) =
    if x > k
        then case a of
                 Leaf -> SumsTo Leaf t
                 Node y aa ab ->
                     if y > k
                         then case partitionLeGt k aa of
                                  SumsTo small big ->
                                      SumsTo small (Node y big (Node x ab b))
                         else case partitionLeGt k ab of
                                  SumsTo small big ->
                                      SumsTo (Node y aa small) (Node x big b)
        else case b of
                 Leaf -> SumsTo t Leaf
                 Node y ba bb ->
                     if y > k
                         then case partitionLeGt k ba of
                                  SumsTo small big ->
                                      SumsTo (Node x a small) (Node y big bb)
                         else case partitionLeGt k bb of
                                  SumsTo small big ->
                                      SumsTo (Node y (Node x a ba) small) big
{-# INLINE partitionLeGt #-}

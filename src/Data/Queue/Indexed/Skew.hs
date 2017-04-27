{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- | Size-indexed skew heaps.
module Data.Queue.Indexed.Skew
  (Skew(..))
  where

import           Data.Queue.Indexed.Class

import           GHC.TypeLits

import           Control.DeepSeq (NFData(rnf))

-- | A size-indexed skew heap.
data Skew n a where
        Empty :: Skew 0 a
        Node :: a -> Skew n a -> Skew m a -> Skew (1 + n + m) a

instance Ord a => IndexedQueue Skew a where
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

instance NFData a =>
         NFData (Skew n a) where
    rnf Empty = ()
    rnf (Node x l r) = rnf x `seq` rnf l `seq` rnf r

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Skew heaps.
module Data.Heap.Skew
  (Skew(..))
  where

import           Data.BinaryTree
import           Data.Heap.Class

import           Control.DeepSeq (NFData(rnf))
import           Data.Data       (Data)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic, Generic1)

-- | A simple, unchecked skew heap.
newtype Skew a = Skew
    { runSkew :: Tree a
    } deriving (Functor,Foldable,Traversable,Data,Typeable,Generic,Generic1)

instance Ord a => Monoid (Skew a) where
    mempty = Skew Leaf
    mappend (Skew xs) (Skew ys) = Skew (smerge xs ys)

smerge :: Ord a => Tree a -> Tree a -> Tree a
smerge Leaf ys = ys
smerge xs Leaf = xs
smerge h1@(Node x lx rx) h2@(Node y ly ry)
  | x <= y    = Node x (smerge h2 rx) lx
  | otherwise = Node y (smerge h1 ry) ly

instance Ord a => Queue Skew a where
    singleton x = Skew (Node x Leaf Leaf)
    minView (Skew Leaf)         = Nothing
    minView (Skew (Node x l r)) = Just (x, Skew (smerge l r))
    empty = mempty
    insert = merge . singleton

instance Ord a => MeldableQueue Skew a where
    merge = mappend

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance NFData a =>
         NFData (Skew a) where
    rnf (Skew x) = rnf x `seq` ()

instance Ord a => Eq (Skew a) where
    (==) = eqQueue

instance Ord a => Ord (Skew a) where
    compare = cmpQueue

instance (Show a, Ord a) => Show (Skew a) where
    showsPrec = showsPrecQueue

instance (Read a, Ord a) => Read (Skew a) where
    readsPrec = readPrecQueue

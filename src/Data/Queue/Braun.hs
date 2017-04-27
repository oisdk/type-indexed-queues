{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Simple, unchecked braun heaps.
module Data.Queue.Braun
  (Braun(..))
  where

import           Data.BinaryTree
import           Data.Queue.Class

import           Control.DeepSeq (NFData (rnf))
import           Data.Data       (Data)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic, Generic1)

-- | A Braun heap. Based on
-- <https://github.com/coq/coq/blob/d8a07b44f5245f8e2f3a47095c70bb3cc85e3d99/lib/heap.ml this implementation>.
--
-- A braun tree is a /nearly balanced/ binary tree: the left branch can
-- be either exactly the same size as the right, or one element larger.
--
-- This version is unchecked (/very/ unchecked), and is provided mainly
-- for comparison to the checked version.
newtype Braun a = Braun
    { runBraun :: Tree a
    } deriving (Typeable,Generic,Data,Generic1,Functor,Foldable,Traversable)

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x Leaf = Node x Leaf Leaf
insertTree x (Node y l r)
    | x <= y    = Node x (insertTree y r) l
    | otherwise = Node y (insertTree x r) l

instance Ord a => Queue Braun a where
    insert x (Braun ys) = Braun (insertTree x ys)
    empty = Braun Leaf
    minView (Braun xs) = (fmap.fmap) Braun (minViewTree xs)

extract :: Ord a => Tree a -> (a, Tree a)
extract (Node y Leaf Leaf) = (y, Leaf)
extract (Node y l r)       = let (x,l') = extract l in (x, Node y r l')
extract Leaf               = error "extract called on empty braun tree"

mergeTree :: Ord a => Tree a -> Tree a -> Tree a
mergeTree xs Leaf = xs
mergeTree Leaf ys = ys
mergeTree l@(Node lx ll lr) r@(Node ly _ _)
  | lx <= ly = Node lx r (mergeTree ll lr)
  | otherwise = let (x,l') = extract l in
    Node ly (replaceMax x r) l'

replaceMax :: Ord a => a -> Tree a -> Tree a
replaceMax x (Node _ l r)
  | isAbove x l, isAbove x r = Node x l r
replaceMax x (Node _ l@(Node lx _ _) r)
  | isAbove lx r = Node lx (replaceMax x l) r
replaceMax x (Node _ l r@(Node rx _ _)) = Node rx l (replaceMax x r)
replaceMax _ _ = error "impossible"

isAbove :: Ord a => a -> Tree a -> Bool
isAbove _ Leaf         = True
isAbove x (Node y _ _) = x <= y

minViewTree :: Ord a => Tree a -> Maybe (a, Tree a)
minViewTree Leaf         = Nothing
minViewTree (Node x l r) = Just (x, mergeTree l r)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance NFData a => NFData (Braun a) where
    rnf (Braun xs) = rnf xs

instance Ord a => Eq (Braun a) where
    (==) = eqQueue

instance Ord a => Ord (Braun a) where
    compare = cmpQueue

instance (Show a, Ord a) => Show (Braun a) where
    showsPrec = showsPrecQueue

instance (Read a, Ord a) => Read (Braun a) where
    readsPrec = readPrecQueue

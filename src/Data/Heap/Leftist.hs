{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Heap.Leftist
  (Leftist(..)
  ,zygoLeftist)
  where

import           Data.Heap.Class

import           Control.DeepSeq (NFData(rnf))
import           Data.Data       (Data)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic, Generic1)

data Leftist a
    = Leaf
    | Node {-# UNPACK #-} !Int
           a
           (Leftist a)
           (Leftist a)
    deriving (Functor,Foldable,Traversable,Data,Typeable,Generic,Generic1)

rank :: Leftist s -> Int
rank Leaf          = 0
rank (Node r _ _ _) = r
{-# INLINE rank #-}

instance Ord a => PriorityQueue Leftist a where

    minView Leaf          = Nothing
    minView (Node _ x l r) = Just (x, merge l r)
    {-# INLINE minView #-}

    singleton x = Node 1 x Leaf Leaf
    {-# INLINE singleton #-}

    empty = Leaf
    {-# INLINE empty #-}

    insert = merge . singleton
    {-# INLINE insert #-}

instance Ord a => MeldableQueue Leftist a where
    merge Leaf h2 = h2
    merge h1 Leaf = h1
    merge t1@(Node _ x1 l1 r1) t2@(Node _ x2 l2 r2)
      | x1 <= x2 = join l1 x1 (merge r1 t2)
      | otherwise = join l2 x2 (merge t1 r2)

join :: Ord a => Leftist a -> a -> Leftist a -> Leftist a
join t1 x t2
  | rank t1 >= rank t2 = Node (rank t2 + 1) x t1 t2
  | otherwise = Node (rank t1 + 1) x t2 t1
{-# INLINE join #-}

instance Ord a => Monoid (Leftist a) where
    mempty = empty
    mappend = merge

zygoLeftist
    :: b1
    -> (Int -> a -> b1 -> b1 -> b1)
    -> b
    -> (Int -> a -> b1 -> b -> b1 -> b -> b)
    -> Leftist a
    -> b
zygoLeftist b1 f1 b f = snd . go
  where
    go Leaf = (b1, b)
    go (Node n x l r) =
        let (lr1,lr) = go l
            (rr1,rr) = go r
        in (f1 n x lr1 rr1, f n x lr1 lr rr1 rr)


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance NFData a =>
         NFData (Leftist a) where
    rnf Leaf = ()
    rnf (Node i x l r) = rnf i `seq` rnf x `seq` rnf l `seq` rnf r `seq` ()

instance Ord a => Eq (Leftist a) where
    (==) = eqQueue

instance Ord a => Ord (Leftist a) where
    compare = cmpQueue

instance (Show a, Ord a) => Show (Leftist a) where
    showsPrec = showsPrecQueue

instance (Read a, Ord a) => Read (Leftist a) where
    readsPrec = readPrecQueue

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Leftist heaps.
module Data.Queue.Leftist
  (Leftist(..)
  ,zygoLeftist)
  where

import           Data.Queue.Class

import           Control.DeepSeq (NFData (rnf))
import           Data.Data       (Data)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic, Generic1)


-- | A simple, unchecked, weight-biased leftist heap. Based on
-- implementation from <https://github.com/jstolarek/dep-typed-wbl-heaps-hs here>.
data Leftist a
    = Leaf
    | Node {-# UNPACK #-} !Int
           a
           (Leftist a)
           (Leftist a)
    deriving (Functor,Foldable,Traversable,Data,Typeable,Generic,Generic1)



rank :: Leftist s -> Int
rank Leaf           = 0
rank (Node r _ _ _) = r
{-# INLINE rank #-}

instance Ord a => Queue Leftist a where

    minView Leaf           = Nothing
    minView (Node _ x l r) = Just (x, merge l r)
    {-# INLINE minView #-}

    singleton x = Node 1 x Leaf Leaf
    {-# INLINE singleton #-}

    empty = Leaf
    {-# INLINE empty #-}

    insert = merge . singleton
    {-# INLINE insert #-}




instance Ord a =>
         MeldableQueue Leftist a where
    merge Leaf h2 = h2
    merge h1 Leaf = h1
    merge h1@(Node w1 p1 l1 r1) h2@(Node w2 p2 l2 r2)
      | p1 < p2 =
          if ll <= lr
              then Node (w1 + w2) p1 l1 (merge r1 h2)
              else Node (w1 + w2) p1 (merge r1 h2) l1
      | otherwise =
          if rl <= rr
              then Node (w1 + w2) p2 l2 (merge r2 h1)
              else Node (w1 + w2) p2 (merge r2 h1) l2
      where
        ll = rank r1 + w2
        lr = rank l1
        rl = rank r2 + w1
        rr = rank l2

instance Ord a => Monoid (Leftist a) where
    mempty = empty
    mappend = merge

-- | A zygomorphism over the heap. Useful for checking shape properties.
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
    rnf Leaf           = ()
    rnf (Node i x l r) = rnf i `seq` rnf x `seq` rnf l `seq` rnf r `seq` ()

instance Ord a => Eq (Leftist a) where
    (==) = eqQueue

instance Ord a => Ord (Leftist a) where
    compare = cmpQueue

instance (Show a, Ord a) => Show (Leftist a) where
    showsPrec = showsPrecQueue

instance (Read a, Ord a) => Read (Leftist a) where
    readsPrec = readPrecQueue

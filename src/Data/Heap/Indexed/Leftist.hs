{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- | Statically verified, type-indexed, weight-biased leftist heaps.
module Data.Heap.Indexed.Leftist
  (Leftist(..))
  where

import           Data.Heap.Indexed.Class

import           Data.Type.Equality
import           Prelude
import           TypeLevel.Bool
import           TypeLevel.Singletons

import           Control.DeepSeq (NFData(rnf))

-- | A size-indexed weight-biased leftist heap. Somewhat based on
-- the implementation from
-- <https://github.com/jstolarek/dep-typed-wbl-heaps-hs here>.
--
-- Type-level natural numbers are used to maintain the invariants
-- in the size of the structure, but these are backed by
-- 'Numeric.Natural.Natural' at runtime, maintaining some
-- efficiency.
--
-- For instance, the '<=.' function is used, which compares two
-- numbers (runtime integers), but provides a boolean singleton
-- as its result: when matched on, this provides a /type-level-proof/
-- of the ordering.
data Leftist n a where
        Leaf :: Leftist 0 a
        Node :: !(The Nat (n + m + 1))
             -> a
             -> Leftist n a
             -> Leftist m a
             -> !(m <= n)
             -> Leftist (n + m + 1) a

rank :: Leftist n s -> The Nat n
rank Leaf             = sing
rank (Node r _ _ _ _) = r
{-# INLINE rank #-}

instance Ord a => IndexedPriorityQueue Leftist a where

    minView (Node _ x l r _) = (x, merge l r)
    {-# INLINE minView #-}


    singleton x = Node sing x Leaf Leaf Refl
    {-# INLINE singleton #-}

    empty = Leaf
    {-# INLINE empty #-}

    insert = merge . singleton
    {-# INLINE insert #-}

    minViewMay Leaf b _             = b
    minViewMay (Node _ x l r _) _ f = f x (merge l r)

instance Ord a =>
         MeldableIndexedQueue Leftist a where
    merge Leaf h2 = h2
    merge h1 Leaf = h1
    merge h1@(Node w1 p1 l1 r1 _) h2@(Node w2 p2 l2 r2 _)
      | p1 < p2 =
          if ll <=. lr
             then Node (w1 +. w2) p1 l1 (merge r1 h2)
             else Node (w1 +. w2) p1 (merge r1 h2) l1 . totalOrder ll lr
      | otherwise =
          if rl <=. rr
              then Node (w1 +. w2) p2 l2 (merge r2 h1)
              else Node (w1 +. w2) p2 (merge r2 h1) l2 . totalOrder rl rr
      where
        ll = rank r1 +. w2
        lr = rank l1
        rl = rank r2 +. w1
        rr = rank l2

instance NFData a =>
         NFData (Leftist n a) where
    rnf Leaf = ()
    rnf (Node n x l r Refl) = n `seq` rnf x `seq` rnf l `seq` rnf r `seq` ()

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- | Size-indexed pairing heaps.
module Data.Heap.Indexed.Pairing
  (Pairing(..)
  ,HVec(..))
  where

import           Data.Heap.Indexed.Class
import           GHC.TypeLits

import Control.DeepSeq (NFData(rnf))

-- | A simple size-indexed pairing heap. In practice, this heap seems
-- to have the best performance.
--
-- Inspired by the implementation <https://github.com/treeowl/sort-traversable here>,
-- but uses type-level literals, rather than type-level Peano numbers.
data Pairing n a where
  E :: Pairing 0 a
  T :: a -> HVec n a -> Pairing (1 + n) a

type role Pairing nominal nominal

-- | A size-indexed vector of pairing heaps.
data HVec n a where
  HNil :: HVec 0 a
  HCons :: Pairing m a -> HVec n a -> HVec (m + n) a

instance Ord a => IndexedQueue Pairing a where
    minView (T x hs) = (x, mergePairs hs)
    {-# INLINABLE minView #-}
    singleton a = T a HNil
    empty = E
    insert = merge . singleton
    {-# INLINABLE insert #-}

    minViewMay E b _        = b
    minViewMay (T x hs) _ f = f x (mergePairs hs)

instance Ord a => MeldableIndexedQueue Pairing a where
    merge E ys = ys
    merge xs E = xs
    merge h1@(T x xs) h2@(T y ys)
      | x <= y = T x (HCons h2 xs)
      | otherwise = T y (HCons h1 ys)
    {-# INLINABLE merge #-}

mergePairs :: Ord a => HVec n a -> Pairing n a
mergePairs HNil = E
mergePairs (HCons h HNil) = h
mergePairs (HCons h1 (HCons h2 hs)) =
    merge (merge h1 h2) (mergePairs hs)
{-# INLINABLE mergePairs #-}

instance NFData a => NFData (Pairing n a) where
    rnf E = ()
    rnf (T x xs) = rnf x `seq` rnf xs `seq` ()

instance NFData a => NFData (HVec n a) where
    rnf HNil = ()
    rnf (HCons x xs) = rnf x `seq` rnf xs `seq` ()

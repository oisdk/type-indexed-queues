{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Pairing heaps.
module Data.Heap.Pairing
  (Pairing(..))
  where

import           Data.Heap.Class

import           Control.DeepSeq (NFData(rnf))
import           Data.Data       (Data)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic, Generic1)

-- | A simple, unchecked pairing heap.
data Pairing a
    = E
    | T a [Pairing a]
    deriving (Functor,Foldable,Traversable,Data,Typeable,Generic,Generic1)

instance Ord a => Monoid (Pairing a) where
    mempty = E
    mappend E ys = ys
    mappend xs E = xs
    mappend h1@(T x xs) h2@(T y ys)
      | x <= y = T x (h2 : xs)
      | otherwise = T y (h1 : ys)
    {-# INLINABLE mappend #-}

instance Ord a => Queue Pairing a where
    singleton a = T a []
    insert = mappend . singleton
    {-# INLINABLE insert #-}
    minView (T x hs) = Just (x, mergePairs hs)
    minView E        = Nothing
    {-# INLINABLE minView #-}
    empty = mempty
    {-# INLINE empty #-}

instance Ord a => MeldableQueue Pairing a where
    merge = mappend
    {-# INLINE merge #-}

mergePairs :: Ord a => [Pairing a] -> Pairing a
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1 : h2 : hs) =
    mappend (mappend h1 h2) (mergePairs hs)
{-# INLINABLE mergePairs #-}

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance NFData a =>
         NFData (Pairing a) where
    rnf E = ()
    rnf (T x xs) = rnf x `seq` rnf xs `seq` ()

instance Ord a => Eq (Pairing a) where
    (==) = eqQueue

instance Ord a => Ord (Pairing a) where
    compare = cmpQueue

instance (Show a, Ord a) => Show (Pairing a) where
    showsPrec = showsPrecQueue

instance (Read a, Ord a) => Read (Pairing a) where
    readsPrec = readPrecQueue

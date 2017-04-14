{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

-- | Size-indexed binomial heaps.
module Data.Heap.Indexed.Binomial
  (Tree(..)
  ,Node(..)
  ,Binomial(..))
  where

import           GHC.TypeLits

import           Data.Heap.Indexed.Class

import           Data.Typeable (Typeable)
import           GHC.Generics (Generic, Generic1)
import           Control.DeepSeq (NFData(rnf))

-- | A size-indexed binomial tree.
data Tree n a = Root a (Node n a)

-- | A binomial tree, indexed by its size.
data Node :: Nat -> * -> * where
        NilN :: Node 0 a
        (:<) :: {-# UNPACK #-} !(Tree n a)
             -> Node n a
             -> Node (1 + n) a

mergeTree :: Ord a => Tree n a -> Tree n a -> Tree (1 + n) a
mergeTree xr@(Root x xs) yr@(Root y ys)
  | x <= y    = Root x (yr :< xs)
  | otherwise = Root y (xr :< ys)
{-# INLINE mergeTree #-}

infixr 5 :-
-- | A size-indexed binomial heap.
--
-- The implementation is similar to:
--
-- * <http://www.cs.ox.ac.uk/ralf.hinze/publications/#J1 Hinze, Ralf. “Functional Pearls: Explaining Binomial Heaps.” Journal of Functional Programming 9, no. 1 (January 1999): 93–104. doi:10.1017/S0956796899003317.>
-- * <https://themonadreader.files.wordpress.com/2010/05/issue16.pdf Wasserman, Louis. “Playing with Priority Queues.” The Monad.Reader, May 12, 2010.>
--
-- However invariants are more aggressively maintained, using a
-- typechecker plugin. It is a list of binomial trees, equivalent to a
-- binary number (stored least-significant-bit first).
data Binomial :: Nat -> Nat -> * -> * where
        Nil  :: Binomial n 0 a
        (:-) :: {-# UNPACK #-} !(Tree z a)
             -> Binomial (1 + z) xs a
             -> Binomial z (1 + xs + xs) a
        Skip :: Binomial (1 + z) (1 + xs) a
             -> Binomial z (2 + xs + xs) a

instance Ord a => IndexedQueue (Binomial 0) a where
    empty = Nil
    minView xs = case minViewZip xs of
      Zipper x _ ys -> (x, ys)
    singleton x = Root x NilN :- Nil
    insert = merge . singleton
    minViewMay q b f = case q of
      Nil -> b
      _ :- _ -> uncurry f (minView q)
      Skip _ -> uncurry f (minView q)

instance Ord a => MeldableIndexedQueue (Binomial 0) a where
    merge = mergeB
    {-# INLINE merge #-}

mergeB
    :: Ord a
    => Binomial z xs a -> Binomial z ys a -> Binomial z (xs + ys) a
mergeB Nil ys              = ys
mergeB xs Nil              = xs
mergeB (Skip xs) (Skip ys) = Skip (mergeB xs ys)
mergeB (Skip xs) (y :- ys) = y :- mergeB xs ys
mergeB (x :- xs) (Skip ys) = x :- mergeB xs ys
mergeB (x :- xs) (y :- ys) = Skip (mergeCarry (mergeTree x y) xs ys)

mergeCarry
    :: Ord a
    => Tree z a
    -> Binomial z xs a
    -> Binomial z ys a
    -> Binomial z (1 + xs + ys) a
mergeCarry !t Nil ys              = carryLonger t ys
mergeCarry !t xs Nil              = carryLonger t xs
mergeCarry !t (Skip xs) (Skip ys) = t :- mergeB xs ys
mergeCarry !t (Skip xs) (y :- ys) = Skip (mergeCarry (mergeTree t y) xs ys)
mergeCarry !t (x :- xs) (Skip ys) = Skip (mergeCarry (mergeTree t x) xs ys)
mergeCarry !t (x :- xs) (y :- ys) = t :- mergeCarry (mergeTree x y) xs ys

carryLonger :: Ord a => Tree z a -> Binomial z xs a -> Binomial z (1 + xs) a
carryLonger !t Nil       = t :- Nil
carryLonger !t (Skip xs) = t :- xs
carryLonger !t (x :- xs) = Skip (carryLonger (mergeTree t x) xs)

data Zipper a n rk = Zipper !a (Node rk a) (Binomial rk n a)

skip :: Binomial (1 + z) xs a -> Binomial z (xs + xs) a
skip x = case x of
  Nil    -> Nil
  Skip _ -> Skip x
  _ :- _ -> Skip x

data MinViewZipper a n rk where
    Infty :: MinViewZipper a 0 rk
    Min :: {-# UNPACK #-} !(Zipper a n rk) -> MinViewZipper a (n+1) rk

slideLeft :: Zipper a n (1 + rk) -> Zipper a (1 + n + n) rk
slideLeft (Zipper m (t :< ts) hs)
  = Zipper m ts (t :- hs)

pushLeft :: Ord a => Tree rk a -> Zipper a n (1 + rk) -> Zipper a (2 + n + n) rk
pushLeft c (Zipper m (t :< ts) hs)
  = Zipper m ts (Skip (carryLonger (mergeTree c t) hs))

minViewZip :: Ord a => Binomial rk (1 + n) a -> Zipper a n rk
minViewZip (Skip xs) = slideLeft (minViewZip xs)
minViewZip (t@(Root x ts) :- f) = case minViewZipMay f of
  Min ex@(Zipper minKey _ _) | minKey < x -> pushLeft t ex
  _                          -> Zipper x ts (skip f)

minViewZipMay :: Ord a => Binomial rk n a -> MinViewZipper a n rk
minViewZipMay (Skip xs) = Min (slideLeft (minViewZip xs))
minViewZipMay Nil = Infty
minViewZipMay (t@(Root x ts) :- f) = Min $ case minViewZipMay f of
  Min ex@(Zipper minKey _ _) | minKey < x -> pushLeft t ex
  _                          -> Zipper x ts (skip f)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance NFData a => NFData (Binomial rk n a) where
    rnf Nil = ()
    rnf (Skip xs) = rnf xs `seq` ()
    rnf (x :- xs) = rnf x `seq` rnf xs `seq` ()

deriving instance Foldable (Binomial rk n)
deriving instance Functor (Binomial rk n)
deriving instance Traversable (Binomial rk n)
deriving instance Typeable a => Typeable (Binomial n a)

deriving instance Foldable (Tree rk)
deriving instance Functor (Tree rk)
deriving instance Traversable (Tree rk)
deriving instance Generic (Tree n a)
deriving instance Generic1 (Tree n)
deriving instance Typeable a => Typeable (Tree n a)

instance NFData a => NFData (Tree rk a) where
    rnf (Root x xs) = rnf x `seq` rnf xs `seq` ()

deriving instance Typeable a => Typeable (Node n a)
deriving instance Foldable (Node rk)
deriving instance Functor (Node rk)
deriving instance Traversable (Node rk)

instance NFData a => NFData (Node rk a) where
    rnf NilN = ()
    rnf (x :< xs) = rnf x `seq` rnf xs `seq` ()

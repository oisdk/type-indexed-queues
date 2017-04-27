{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}

-- | Simple binomial heaps, with a statically-enforced shape.
module Data.Queue.Binomial
  (Binomial(..)
  ,Node(..)
  ,Tree(..))
  where

import           TypeLevel.Nat
import           Data.Queue.Class

import           Data.Typeable (Typeable)
import           GHC.Generics (Generic, Generic1)
import           Control.DeepSeq (NFData(rnf))

infixr 5 :-
-- | A binomial heap, where the sizes of the nodes are enforced in the types.
--
-- The implementation is based on:
--
-- * <http://www.cs.ox.ac.uk/ralf.hinze/publications/#J1 Hinze, Ralf. “Functional Pearls: Explaining Binomial Heaps.” Journal of Functional Programming 9, no. 1 (January 1999): 93–104. doi:10.1017/S0956796899003317.>
-- * <https://themonadreader.files.wordpress.com/2010/05/issue16.pdf Wasserman, Louis. “Playing with Priority Queues.” The Monad.Reader, May 12, 2010.>
--
-- It is a list of binomial trees, equivalent to a binary number (stored
-- least-significant-bit first).

data Binomial rk a
    -- | The empty heap
    = Nil
    -- | Skip a child tree (equivalent to a zero in the binary representation
    -- of the data structure).
    | Skip (Binomial ('S rk) a)
    -- | A child tree. Equivalent to a one in the binary representation.
    | (:-) {-# UNPACK #-} !(Tree rk a)
           (Binomial ('S rk) a)

-- | A rose tree, where the children are indexed.
data Tree rk a = Root a (Node rk a)

-- | A list of binomial trees, indexed by their sizes in ascending order.
data Node n a where
        NilN :: Node 'Z a
        (:<) :: {-# UNPACK #-} !(Tree n a) -> Node n a -> Node ('S n) a

mergeTree :: Ord a => Tree rk a -> Tree rk a -> Tree ('S rk) a
mergeTree xr@(Root x xs) yr@(Root y ys)
  | x <= y    = Root x (yr :< xs)
  | otherwise = Root y (xr :< ys)

instance Ord a =>
         Monoid (Binomial rk a) where
    mappend Nil ys              = ys
    mappend xs Nil              = xs
    mappend (Skip xs) (Skip ys) = Skip (mappend xs ys)
    mappend (Skip xs) (y :- ys) = y :- mappend xs ys
    mappend (x :- xs) (Skip ys) = x :- mappend xs ys
    mappend (x :- xs) (y :- ys) = Skip (mergeCarry (mergeTree x y) xs ys)
    mempty = Nil

mergeCarry
    :: Ord a
    => Tree rk a -> Binomial rk a -> Binomial rk a -> Binomial rk a
mergeCarry !t Nil ys              = carryLonger t ys
mergeCarry !t xs Nil              = carryLonger t xs
mergeCarry !t (Skip xs) (Skip ys) = t :- mappend xs ys
mergeCarry !t (Skip xs) (y :- ys) = Skip (mergeCarry (mergeTree t y) xs ys)
mergeCarry !t (x :- xs) (Skip ys) = Skip (mergeCarry (mergeTree t x) xs ys)
mergeCarry !t (x :- xs) (y :- ys) = t :- mergeCarry (mergeTree x y) xs ys

carryLonger :: Ord a => Tree rk a -> Binomial rk a -> Binomial rk a
carryLonger !t Nil       = t :- Nil
carryLonger !t (Skip xs) = t :- xs
carryLonger !t (x :- xs) = Skip (carryLonger (mergeTree t x) xs)

data Zipper a rk = Zipper (Node rk a) (Binomial rk a)

data MinViewZipper a rk
    = Infty
    | Min !a {-# UNPACK #-} !(Zipper a rk)

slideLeft :: Zipper a ('S rk) -> Zipper a rk
slideLeft (Zipper (t :< ts) hs) = Zipper ts (t :- hs)

pushLeft :: Ord a => Tree rk a -> Zipper a ('S rk) -> Zipper a rk
pushLeft t (Zipper (x :< xs) ts)
  = Zipper xs (Skip (carryLonger (mergeTree t x) ts))

minViewZip :: Ord a => Binomial rk a -> MinViewZipper a rk
minViewZip Nil = Infty
minViewZip (Skip xs) = case minViewZip xs of
  Infty   -> Infty
  Min e x -> Min e (slideLeft x)
minViewZip (t@(Root x ts) :- f) =
    case minViewZip f of
        Min minKey ex
          | minKey < x -> Min minKey (pushLeft t ex)
        _ -> Min x (Zipper ts (Skip f))

instance Ord a => Queue (Binomial 'Z) a where
    minView hs =
        case minViewZip hs of
            Infty               -> Nothing
            Min x (Zipper _ ts) -> Just (x, ts)
    singleton x = Root x NilN :- Nil
    insert x = carryLonger (Root x NilN)
    empty = mempty
    {-# INLINE empty #-}

instance Ord a => MeldableQueue (Binomial 'Z) a where
    merge = mappend
    {-# INLINE merge #-}

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance NFData a => NFData (Binomial rk a) where
    rnf Nil = ()
    rnf (Skip xs) = rnf xs
    rnf (x :- xs) = rnf x `seq` rnf xs

deriving instance Foldable (Binomial rk)
deriving instance Functor (Binomial rk)
deriving instance Traversable (Binomial rk)
deriving instance Generic (Binomial n a)
deriving instance Generic1 (Binomial n)
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

instance Ord a => Eq (Binomial 'Z a) where
    (==) = eqQueue

instance Ord a => Ord (Binomial 'Z a) where
    compare = cmpQueue

instance (Show a, Ord a) => Show (Binomial 'Z a) where
    showsPrec = showsPrecQueue

instance (Read a, Ord a) => Read (Binomial 'Z a) where
    readsPrec = readPrecQueue

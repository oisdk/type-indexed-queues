{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Data.Heap.Binomial where

import           TypeLevel.Nat

import           Data.Heap.Class

infixr 5 :-
data Binomial rk a
    = Nil
    | Skip (Binomial ('S rk) a)
    | (:-) {-# UNPACK #-} !(Tree rk a)
           (Binomial ('S rk) a)

data Tree rk a = Root a (Node rk a)

deriving instance Foldable (Tree rk)
deriving instance Foldable (Node rk)

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

instance Ord a => PriorityQueue (Binomial 'Z) a where
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

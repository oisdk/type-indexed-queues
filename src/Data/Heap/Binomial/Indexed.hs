{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE BangPatterns        #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module Data.Heap.Binomial.Indexed where

import GHC.TypeLits

data Tree n a = Root a (Node n a)

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
data Heap :: Nat -> Nat -> * -> * where
        Nil  :: Heap n 0 a
        (:-) :: {-# UNPACK #-} !(Tree z a)
             -> Heap (1 + z) xs a
             -> Heap z (1 + xs + xs) a
        Skip :: Heap (1 + z) (1 + xs) a
             -> Heap z (2 + xs + xs) a

merge :: Ord a => Heap z xs a -> Heap z ys a -> Heap z (xs + ys) a
merge Nil ys = ys
merge xs Nil = xs
merge (Skip xs) (Skip ys) = Skip (merge xs ys)
merge (Skip xs) (y :- ys) = y :- merge xs ys
merge (x :- xs) (Skip ys) = x :- merge xs ys
merge (x :- xs) (y :- ys) = Skip (mergeCarry (mergeTree x y) xs ys)

mergeCarry :: Ord a => Tree z a -> Heap z xs a -> Heap z ys a -> Heap z (1 + xs + ys) a
mergeCarry !t Nil ys = carryLonger t ys
mergeCarry !t xs Nil = carryLonger t xs
mergeCarry !t (Skip xs) (Skip ys) = t :- merge xs ys
mergeCarry !t (Skip xs) (y :- ys) = Skip (mergeCarry (mergeTree t y) xs ys)
mergeCarry !t (x :- xs) (Skip ys) = Skip (mergeCarry (mergeTree t x) xs ys)
mergeCarry !t (x :- xs) (y :- ys) = t :- mergeCarry (mergeTree x y) xs ys

carryLonger :: Ord a => Tree z a -> Heap z xs a -> Heap z (1 + xs) a
carryLonger !t Nil = t :- Nil
carryLonger !t (Skip xs) = t :- xs
carryLonger !t (x :- xs) = Skip (carryLonger (mergeTree t x) xs)

data Zipper a n rk = Zipper !a (Node rk a) (Heap rk n a)

skip :: Heap (1 + z) xs a -> Heap z (xs + xs) a
skip x = case x of
  Nil -> Nil
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

minViewZip :: Ord a => Heap rk (1 + n) a -> Zipper a n rk
minViewZip (Skip xs) = slideLeft (minViewZip xs)
minViewZip (t@(Root x ts) :- f) = case minViewMay f of
  Min ex@(Zipper minKey _ _) | minKey < x -> pushLeft t ex
  _ -> Zipper x ts (skip f)

minViewMay :: Ord a => Heap rk n a -> MinViewZipper a n rk
minViewMay (Skip xs) = Min (slideLeft (minViewZip xs))
minViewMay Nil = Infty
minViewMay (t@(Root x ts) :- f) = Min $ case minViewMay f of
  Min ex@(Zipper minKey _ _) | minKey < x -> pushLeft t ex
  _ -> Zipper x ts (skip f)

minView :: Ord a => Heap rk (1 + n) a -> (a, Heap rk n a)
minView xs = case minViewZip xs of
  Zipper x _ ys -> (x, ys)


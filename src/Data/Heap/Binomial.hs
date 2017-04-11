{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}

module Data.Heap.Binomial where

import           Data.List     (unfoldr)

import           TypeLevel.Nat

infixr 5 :-
data Heap rk a
    = Nil
    | Skip (Heap ('S rk) a)
    | (:-) {-# UNPACK #-} !(Tree rk a)
           (Heap ('S rk) a)

data Tree rk a = Root a (Node rk a)

data Node n a where
        NilN :: Node 'Z a
        (:<) :: {-# UNPACK #-} !(Tree n a) -> Node n a -> Node ('S n) a

mergeTree :: Ord a => Tree rk a -> Tree rk a -> Tree ('S rk) a
mergeTree xr@(Root x xs) yr@(Root y ys)
  | x <= y    = Root x (yr :< xs)
  | otherwise = Root y (xr :< ys)

merge :: Ord a => Heap rk a -> Heap rk a -> Heap rk a
merge Nil ys              = ys
merge xs Nil              = xs
merge (Skip xs) (Skip ys) = Skip (merge xs ys)
merge (Skip xs) (y :- ys) = y :- merge xs ys
merge (x :- xs) (Skip ys) = x :- merge xs ys
merge (x :- xs) (y :- ys) = Skip (mergeCarry (mergeTree x y) xs ys)

mergeCarry :: Ord a => Tree rk a -> Heap rk a -> Heap rk a -> Heap rk a
mergeCarry !t Nil ys              = carryLonger t ys
mergeCarry !t xs Nil              = carryLonger t xs
mergeCarry !t (Skip xs) (Skip ys) = t :- merge xs ys
mergeCarry !t (Skip xs) (y :- ys) = Skip (mergeCarry (mergeTree t y) xs ys)
mergeCarry !t (x :- xs) (Skip ys) = Skip (mergeCarry (mergeTree t x) xs ys)
mergeCarry !t (x :- xs) (y :- ys) = t :- mergeCarry (mergeTree x y) xs ys

carryLonger :: Ord a => Tree rk a -> Heap rk a -> Heap rk a
carryLonger !t Nil       = t :- Nil
carryLonger !t (Skip xs) = t :- xs
carryLonger !t (x :- xs) = Skip (carryLonger (mergeTree t x) xs)

instance Ord a => Monoid (Heap rk a) where
    mempty = Nil
    {-# INLINE mempty #-}
    mappend = merge
    {-# INLINE mappend #-}

data Zipper a rk = Zipper (Node rk a) (Heap rk a)

data MinViewZipper a rk
    = Infty
    | Min !a {-# UNPACK #-} !(Zipper a rk)

slideLeft :: Zipper a ('S rk) -> Zipper a rk
slideLeft (Zipper (t :< ts) hs) = Zipper ts (t :- hs)

pushLeft :: Ord a => Tree rk a -> Zipper a ('S rk) -> Zipper a rk
pushLeft t (Zipper (x :< xs) ts)
  = Zipper xs (Skip (carryLonger (mergeTree t x) ts))

minViewZip :: Ord a => Heap rk a -> MinViewZipper a rk
minViewZip Nil = Infty
minViewZip (Skip xs) = case minViewZip xs of
  Infty   -> Infty
  Min e x -> Min e (slideLeft x)
minViewZip (t@(Root x ts) :- f) =
    case minViewZip f of
        Min minKey ex
          | minKey < x -> Min minKey (pushLeft t ex)
        _ -> Min x (Zipper ts (Skip f))

minView :: Ord a => Heap rk a -> Maybe (a, Heap rk a)
minView hs =
    case minViewZip hs of
        Infty               -> Nothing
        Min x (Zipper _ ts) -> Just (x, ts)

newtype BinHeap a = BinHeap { runBinHeap :: Heap 'Z a }

{-# INLINE toList #-}
toList :: Ord a => BinHeap a -> [a]
toList (BinHeap xs) = unfoldr minView xs

{-# INLINE insert #-}
insert :: Ord a => a -> Heap 'Z a -> Heap 'Z a
insert x = carryLonger (Root x NilN)

{-# INLINE fromList #-}
fromList :: Ord a => [a] -> BinHeap a
fromList = BinHeap . foldr insert Nil

{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Heap.WithOrdDict where

import           Data.Heap.Class
import           Data.Proxy

data WithOrdDict f a where
    WithOrdDict :: Ord a => f a -> WithOrdDict f a

instance PriorityQueue f => PriorityQueue (WithOrdDict f) where
    minView (WithOrdDict xs) = (fmap.fmap) WithOrdDict (minView xs)
    insert x (WithOrdDict xs) = WithOrdDict (insert x xs)
    empty = WithOrdDict empty
    singleton = WithOrdDict . singleton
    toList (WithOrdDict xs) = toList xs
    fromList = WithOrdDict . fromList
    heapSort (_ :: p (WithOrdDict h)) = heapSort (Proxy :: Proxy h)

instance MeldableQueue f => MeldableQueue (WithOrdDict f) where
    merge (WithOrdDict xs) (WithOrdDict ys) = WithOrdDict (merge xs ys)
    fromFoldable = WithOrdDict . fromFoldable

instance (MeldableQueue f, Ord a) => Monoid (WithOrdDict f a) where
    mempty = WithOrdDict empty
    mappend = merge

instance PriorityQueue f => Foldable (WithOrdDict f) where
    foldr f b (WithOrdDict xs) = go xs where
      go hs = case minView hs of
        Nothing -> b
        Just (y,ys) -> f y (go ys)
    foldMap f (WithOrdDict xs) = go xs where
      go hs = case minView hs of
        Nothing -> mempty
        Just (y,ys) -> f y `mappend` go ys

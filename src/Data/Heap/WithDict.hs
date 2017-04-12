{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Heap.WithDict where

import           Data.Heap.Class
import           Data.Proxy

data WithDict f a where
    WithDict :: Suitable f a => f a -> WithDict f a

instance PriorityQueue f => PriorityQueue (WithDict f) where
    type Suitable (WithDict f) a = Suitable f a
    minView (WithDict xs) = (fmap.fmap) WithDict (minView xs)
    insert x (WithDict xs) = WithDict (insert x xs)
    empty = WithDict empty
    singleton = WithDict . singleton
    toList (WithDict xs) = toList xs
    fromList = WithDict . fromList
    heapSort (_ :: p (WithDict h)) = heapSort (Proxy :: Proxy h)

instance MeldableQueue f => MeldableQueue (WithDict f) where
    merge (WithDict xs) (WithDict ys) = WithDict (merge xs ys)
    fromFoldable = WithDict . fromFoldable

instance PriorityQueue f => Foldable (WithDict f) where
    foldr f b (WithDict xs) = go xs where
      go hs = case minView hs of
        Nothing -> b
        Just (y,ys) -> f y (go ys)
    foldMap f (WithDict xs) = go xs where
      go hs = case minView hs of
        Nothing -> mempty
        Just (y,ys) -> f y `mappend` go ys

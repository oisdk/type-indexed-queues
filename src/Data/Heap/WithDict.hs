{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Heap.WithDict
  (WithDict(..))
  where

import           Data.Heap.Class
import           Data.Proxy

data WithDict f a where
    WithDict :: PriorityQueue f a => f a -> WithDict f a

instance PriorityQueue f a => PriorityQueue (WithDict f) a where
    minView (WithDict xs) = (fmap.fmap) WithDict (minView xs)
    insert x (WithDict xs) = WithDict (insert x xs)
    empty = WithDict empty
    singleton = WithDict . singleton
    toList (WithDict xs) = toList xs
    fromList = WithDict . fromList
    heapSort (_ :: p (WithDict h)) = heapSort (Proxy :: Proxy h)

instance MeldableQueue f a => MeldableQueue (WithDict f) a where
    merge (WithDict xs) (WithDict ys) = WithDict (merge xs ys)
    fromFoldable = WithDict . fromFoldable

instance Foldable (WithDict f) where
    foldr f b (WithDict xs) = go xs where
      go hs = case minView hs of
        Nothing     -> b
        Just (y,ys) -> f y (go ys)
    foldMap f (WithDict xs) = go xs where
      go hs = case minView hs of
        Nothing     -> mempty
        Just (y,ys) -> f y `mappend` go ys

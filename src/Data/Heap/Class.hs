{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Heap.Class where

import Data.List (unfoldr)

class Monoid (h a) => MinHeap h a where
    {-# MINIMAL minView, singleton #-}
    minView :: h a -> Maybe (a, h a)
    singleton :: a -> h a
    insert :: a -> h a -> h a
    insert = mappend . singleton

toList :: MinHeap h a => h a -> [a]
toList = unfoldr minView

fromList :: MinHeap h a => [a] -> h a
fromList = foldr insert mempty

fromFoldable :: (MinHeap h a, Foldable f) => f a -> h a
fromFoldable = foldMap singleton

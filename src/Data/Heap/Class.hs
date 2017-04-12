{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Heap.Class where

import Data.List (unfoldr)
import GHC.Exts (Constraint)

class PriorityQueue h  where

    type Suitable h a :: Constraint
    type Suitable h a = Ord a

    {-# MINIMAL minView , insert , empty #-}

    minView
        :: Suitable h a
        => h a -> Maybe (a, h a)

    insert
        :: Suitable h a
        => a -> h a -> h a
    empty
        :: Suitable h a
        => h a
    singleton
        :: Suitable h a
        => a -> h a
    singleton = flip insert empty

    toList :: Suitable h a => h a -> [a]
    toList = unfoldr minView

    fromList :: Suitable h a => [a] -> h a
    fromList = foldr insert empty

    heapSort :: Suitable h a => p h -> [a] -> [a]
    heapSort (_ :: p h) = toList . (fromList :: Suitable h a => [a] -> h a)

class PriorityQueue h => MeldableQueue h where
    merge :: Suitable h a => h a -> h a -> h a

    fromFoldable :: (Foldable f, Suitable h a) => f a -> h a

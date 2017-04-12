{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}

module Data.Heap.Indexed.Erased where

import           GHC.TypeLits

import           Data.Heap.Class
import           Data.Heap.Indexed.Class (IndexedPriorityQueue, MeldableIndexedQueue)
import qualified Data.Heap.Indexed.Class as Indexed

data ErasedSize f a = forall (n :: Nat). ErasedSize
    { runErasedSize :: f n a
    }

instance IndexedPriorityQueue h a =>
         PriorityQueue (ErasedSize h) a where
    insert x (ErasedSize xs) = ErasedSize (Indexed.insert x xs)
    empty = ErasedSize Indexed.empty
    minView (ErasedSize xs) =
        Indexed.minViewMay
            xs
            Nothing
            (\y ys ->
                  Just (y, ErasedSize ys))
    fromList = go Indexed.empty
      where
        go
            :: forall h n a.
               (IndexedPriorityQueue h a)
            => h n a -> [a] -> ErasedSize h a
        go !h [] = ErasedSize h
        go !h (x:xs) = go (Indexed.insert x h) xs


instance MeldableIndexedQueue h a => MeldableQueue (ErasedSize h) a where
    merge (ErasedSize xs) (ErasedSize ys) = ErasedSize (Indexed.merge xs ys)

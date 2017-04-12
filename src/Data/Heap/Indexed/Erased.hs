{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}

module Data.Heap.Indexed.Erased where

import           GHC.TypeLits

import           Data.Heap.Class
import           Data.Heap.Indexed.Class (IndexedPriorityQueue, MeldableIndexedQueue)
import qualified Data.Heap.Indexed.Class as Indexed

data ErasedSize f a = forall (n :: Nat). ErasedSize
    { runErasedSize :: f n a
    }

instance IndexedPriorityQueue h => PriorityQueue (ErasedSize h) where
    insert x (ErasedSize xs) = ErasedSize (Indexed.insert x xs)
    empty = ErasedSize Indexed.empty
    minView (ErasedSize xs) = Indexed.minViewMay xs Nothing (\y ys -> Just (y, ErasedSize ys))

instance MeldableIndexedQueue h => MeldableQueue (ErasedSize h) where
    merge (ErasedSize xs) (ErasedSize ys) = ErasedSize (Indexed.merge xs ys)

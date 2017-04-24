{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}

-- | Erase the size parameter on a size-indexed heap, using existentials.
module Data.Queue.Indexed.Erased
  (ErasedSize(..))
  where

import           GHC.TypeLits

import           Data.Queue.Class
import           Data.Queue.Indexed.Class (IndexedQueue, MeldableIndexedQueue)
import qualified Data.Queue.Indexed.Class as Indexed

-- | This type contains a size-indexed heap, however the size index is
-- hidden. This allows it to act like a standard heap, while maintaining
-- the proven invariants of the size-indexed version.
data ErasedSize f a = forall (n :: Nat). ErasedSize
    { runErasedSize :: f n a
    }

instance IndexedQueue h a =>
         Queue (ErasedSize h) a where
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
               (IndexedQueue h a)
            => h n a -> [a] -> ErasedSize h a
        go !h [] = ErasedSize h
        go !h (x:xs) = go (Indexed.insert x h) xs


instance MeldableIndexedQueue h a => MeldableQueue (ErasedSize h) a where
    merge (ErasedSize xs) (ErasedSize ys) = ErasedSize (Indexed.merge xs ys)

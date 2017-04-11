{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Heap.Class.Indexed where

import           GHC.TypeLits

class IndexedHeap h a where
    empty :: h 0 a
    merge :: h n a -> h m a -> h (n + m) a
    minView :: h (1 + n) a -> (a, h n a)
    singleton :: a -> h 1 a

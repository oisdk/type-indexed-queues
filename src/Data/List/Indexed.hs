{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}


-- | This module exists to showcase some uses for indexed non-priority
-- queues.
module Data.List.Indexed where

import           Data.Heap.Indexed.Class
import           TypeLevel.Singletons hiding (The(..))
import           Data.Traversable.Parts

-- | A simple length-indexed list.
infixr 5 :-
data List n a where
    Nil :: List 0 a
    (:-) :: a -> List n a -> List (1 + n) a

instance IndexedQueue List a where
    empty = Nil
    insert = (:-)
    minView (x :- xs) = (x,xs)
    minViewMay Nil b _ = b
    minViewMay (x :- xs) _ f = f x xs

instance MeldableIndexedQueue List a where
    merge Nil ys = ys
    merge (x :- xs) ys = x :- merge xs ys

-- | A list with efficient concatenation.
newtype DiffList n a = DiffList
    { runDiffList :: forall m. List m a -> List (n + m) a
    }

instance IndexedQueue DiffList a where
    empty = DiffList id
    insert x xs =
        DiffList
            (\ys ->
                  runDiffList xs (x :- ys))
    minView (DiffList xs) =
        case minView (xs Nil) of
            (y,ys) -> (y, DiffList (merge ys))
    minViewMay (DiffList xs) b f =
        minViewMay
            (xs Nil)
            b
            (\y ys ->
                  f y (DiffList (merge ys)))

-- | Performs merging in reverse order.
instance MeldableIndexedQueue DiffList a where
    merge (DiffList xs) (DiffList ys) = DiffList (ys . xs)

-- | Efficiently reverse any traversable, safely and totally.
reverseTraversable :: Traversable t => t a -> t a
reverseTraversable = transformTraversable (`runDiffList` Nil)

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

-- | Sort any traversable. Idea from <https://github.com/treeowl/sort-traversable here>,
-- but parameterized over the heap type.
module Data.Traversable.Sort
  (Sort(..)
  ,liftSort
  ,sortTraversable
  ,runSort
  ,sortTraversal
  ,runSortWith
  ,transformTraversable)
  where

import           Data.Heap.Indexed.Class
import           GHC.TypeLits

-- | A heap with a certain number of elements, and a function which
-- extracts exactly that many elements from a larger heap.
data Sort f a b r where
    Sort :: (forall n. f (m + n) b -> (f n b, r))
         -> !(f m a)
         -> Sort f a b r

instance Functor (Sort f a b) where
  fmap f (Sort g h) =
    Sort (\h' -> case g h' of (remn, r) -> (remn, f r)) h
  {-# INLINE fmap #-}

instance (IndexedQueue f x, MeldableIndexedQueue f x) => Applicative (Sort f x y) where
  pure x = Sort (\h -> (h, x)) empty
  {-# INLINE pure #-}

  (Sort f (xs :: f m x) :: Sort f x y (a -> b)) <*> Sort g (ys :: f n x) =
    Sort h (merge xs ys)
    where
      h :: forall o . f ((m + n) + o) y -> (f o y, b)
      h v = case f v of { (v', a) ->
                case g v' of { (v'', b) ->
                  (v'', a b)}}
  {-# INLINABLE (<*>) #-}

-- | Lift a value into the running sort.
liftSort :: (IndexedQueue f a, IndexedQueue f x) => x -> Sort f x a a
liftSort a = Sort (\h -> case minView h of (x, h') -> (h', x)) (singleton a)
{-# INLINABLE liftSort #-}

-- | Run the built-up function on the stored heap.
runSort :: forall a b f. Sort f b b a -> a
runSort (Sort (f :: f (m + 0) b -> (f 0 b, a)) xs) = snd $ f xs

runSortWith :: forall a b c f. (forall n. f n a -> f n b) -> Sort f a b c -> c
runSortWith f (Sort (g :: f (m + 0) b -> (f 0 b, c)) xs) = snd $ g (f xs)

-- | Sort an arbitrary traversable container using a heap. The first
-- parameter is a phantom, specifying which heap to use.
sortTraversable :: (MeldableIndexedQueue f a, Traversable t) => p f -> t a -> t a
sortTraversable (_ :: p f) =
    runSort .
    traverse
        (liftSort :: (IndexedQueue f x) =>
                     x -> Sort f x x x)
{-# INLINABLE sortTraversable #-}

transformTraversable :: (MeldableIndexedQueue f a, IndexedQueue f b, Traversable t) => (forall n. f n a -> f n b) -> t a -> t b
transformTraversable f = runSortWith f . traverse liftSort

-- | Sort a traversal.
sortTraversal
    :: (IndexedQueue f a)
    => ((a -> Sort f a a a) -> t -> Sort f a a t) -> t -> t
sortTraversal trav = runSort . trav liftSort
{-# INLINABLE sortTraversal #-}


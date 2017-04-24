{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

-- | Sort any traversable. Idea from <https://github.com/treeowl/sort-traversable here>,
-- but parameterized over the heap type.
--
-- Parts can be thought of as a safe version of @unsafePartsOf@ from lens.
module Data.Traversable.Parts
  (Parts(..)
  ,liftParts
  ,queueTraversable
  ,runParts
  ,queueTraversal
  ,runPartsWith
  ,transformTraversal
  ,transformTraversable)
  where

import           Data.Queue.Indexed.Class
import           GHC.TypeLits

-- | A queue with a certain number of elements, and a function which
-- extracts exactly that many elements from a larger queue.
-- You can transform the queue (i.e., reversing, etc.) before running
-- the function, effectively transforming the contents of a traversable
-- safely. If the underlying queue is a priority queue, then inserting
-- elements will sort them as you go.
data Parts f g a b r where
    Parts :: (forall n. g (m + n) b -> (g n b, r))
         -> !(f m a)
         -> Parts f g a b r

instance Functor (Parts f g a b) where
  fmap f (Parts g h) =
    Parts (\h' -> case g h' of (remn, r) -> (remn, f r)) h
  {-# INLINE fmap #-}

instance (IndexedQueue f x, MeldableIndexedQueue f x) =>
          Applicative (Parts f g x y) where
    pure x = Parts (\h -> (h, x)) empty
    {-# INLINE pure #-}

    (Parts f (xs :: f m x) :: Parts f g x y (a -> b)) <*> Parts g (ys :: f n x) =
      Parts h (merge xs ys)
      where
        h :: forall o . g ((m + n) + o) y -> (g o y, b)
        h v = case f v of { (v', a) ->
                  case g v' of { (v'', b) ->
                    (v'', a b)}}
    {-# INLINABLE (<*>) #-}

-- | Lift a value into the running queue.
liftParts :: (IndexedQueue g a, IndexedQueue f x) => x -> Parts f g x a a
liftParts a = Parts (\h -> case minView h of (x, h') -> (h', x)) (singleton a)
{-# INLINABLE liftParts #-}

-- | Run the built-up function on the stored queue.
runParts :: forall a b f. Parts f f b b a -> a
runParts (Parts (f :: f (m + 0) b -> (f 0 b, a)) xs) = snd $ f xs

-- | Perform a length-preserving transformation on the stored queue, and
-- run the built-up function on the transformed version.
runPartsWith :: forall a b c f g. (forall n. f n a -> g n b) -> Parts f g a b c -> c
runPartsWith f (Parts (g :: g (m + 0) b -> (g 0 b, c)) xs) = snd $ g (f xs)

-- | Enqueue every element of a traversable into a queue, and then
-- dequeue them back into the same traversable. This is useful if, for
-- instance, the queue is a priority queue: then this function will
-- perform a sort. If the queue is first-in last-out, this function will
-- perform a reversal.
queueTraversable :: (MeldableIndexedQueue f a, Traversable t) => p f -> t a -> t a
queueTraversable (_ :: p f) =
    runParts .
    traverse
        (liftParts :: (IndexedQueue g x, IndexedQueue f x) =>
                     x -> Parts f g x x x)
{-# INLINABLE queueTraversable #-}

-- | Apply a function which transforms a queue without changing its
-- size to an arbitrary traversable.
transformTraversable
    :: (MeldableIndexedQueue f a, IndexedQueue g b, Traversable t)
    => (forall n. f n a -> g n b) -> t a -> t b
transformTraversable f = runPartsWith f . traverse liftParts

-- | Perform an arbitrary length-preserving transformation
-- on a lens-style traversal.
transformTraversal
    :: (IndexedQueue g b, IndexedQueue f a)
    => (forall n. f n a -> g n b)
    -> ((a -> Parts f g a b b) -> t -> Parts f g a b t)
    -> t
    -> t
transformTraversal f trav = runPartsWith f . trav liftParts


-- | Queues a traversal.
queueTraversal
    :: (IndexedQueue f b, IndexedQueue f a)
    => ((a -> Parts f f a b b) -> t -> Parts f f a a t) -> t -> t
queueTraversal trav = runParts . trav liftParts
{-# INLINABLE queueTraversal #-}


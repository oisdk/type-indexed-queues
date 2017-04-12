{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module Data.Traversable.Sort where

import           Data.Heap.Indexed.Class
import           GHC.TypeLits
import           Data.Proxy

data Sort f a r where
    Sort :: {-# UNPACK #-} !(Proxy o)
         -> !(f m a)
         -> (forall n. f (m + n) a -> (f (o + n) a, r))
         -> Sort f a r

instance Functor (Sort f x) where
  fmap f (Sort o h g) =
    Sort o h (\h' -> case g h' of (remn, r) -> (remn, f r))
  {-# INLINE fmap #-}

instance (MeldableIndexedQueue f, Ord x) =>
         Applicative (Sort f x) where
    pure x =
        Sort
            (Proxy :: Proxy 0)
            empty
            (\h ->
                  (h, x))
    {-# INLINE pure #-}
    {-# INLINABLE (<*>) #-}
    Sort o1 xs f <*> Sort o2 ys g =
        Sort (sumProx o1 o2)  (merge xs ys) (mergeFuncs o1 o2 f g)
      where
        sumProx :: Proxy a -> Proxy b -> Proxy (a + b)
        sumProx _ _ = Proxy
        mergeFuncs
            :: forall a i y m1 m2 o1 o2.
               Proxy o1
            -> Proxy o2
            -> (forall n. f (m1 + n) a -> (f (o1 + n) a, i -> y))
            -> (forall n. f (m2 + n) a -> (f (o2 + n) a, i))
            -> (forall n. f ((m1 + m2) + n) a -> (f (o1 + o2 + n) a, y))
        mergeFuncs _ _ ff gg h =
            case ff h of
                (h1,a) ->
                    case gg h1 of
                        (h2,b) -> (h2, a b)


liftSort :: (IndexedPriorityQueue f, Ord x) => x -> Sort f x x
liftSort a =
    Sort
        (Proxy :: Proxy 0)
        (singleton a)
        (\h ->
              case minView h of
                  (x,h') -> (h', x))
{-# INLINABLE liftSort #-}

runSort :: forall x a f. Sort f x a -> a
runSort (Sort o xs f) = runSort' o (const f) xs
  where
    runSort'
        :: forall m2 aa xx o2.
           Proxy o2
        -> (forall n. Proxy n -> f (m2 + n) aa -> (f (o2 + n) aa, xx))
        -> f m2 aa
        -> xx
    runSort' _ ff h =
        case ff (Proxy :: Proxy 0) h of
            (_,y) -> y

sortTraversable :: (MeldableIndexedQueue f, Traversable t, Ord a) => p f -> t a -> t a
sortTraversable (_ :: p f) =
    runSort .
    traverse
        (liftSort :: Ord x => x -> Sort f x x)
{-# INLINABLE sortTraversable #-}

sortTraversal :: (Ord a, IndexedPriorityQueue f) => ((a -> Sort f a a) -> t -> Sort f a t) -> t -> t
sortTraversal trav = runSort . trav liftSort
{-# INLINABLE sortTraversal #-}

push :: (IndexedPriorityQueue f, Ord a) => a -> Sort f a ()
push x =
    Sort
        (Proxy :: Proxy 1)
        (singleton x)
        (\h ->
              (h, ()))

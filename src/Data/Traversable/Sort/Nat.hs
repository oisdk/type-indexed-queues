{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PolyKinds             #-}

module Data.Traversable.Sort.Nat where

import           TypeLevel.Nat
import           TypeLevel.Nat.Proofs
import           Data.Proxy
import Data.Type.Equality

class IndexedHeap f a where
    merge :: f n a -> f m a -> f (n + m) a
    empty :: f 'Z a
    minView :: f ('S n) a -> (a, f n a)
    singleton :: a -> f ('S 'Z) a
    size :: f n a -> The Nat n

data Sort f a r where
    Sort :: (forall n. Proxy n -> f (m + n) a -> (f n a, r))
         -> !(f m a)
         -> Sort f a r

instance Functor (Sort f x) where
  fmap f (Sort g h) =
    Sort (\p h' -> case g p h' of (remn, r) -> (remn, f r)) h
  {-# INLINE fmap #-}

newtype Flip f (a :: k1) (b :: k2) = Flip { unFlip :: f b a }

instance IndexedHeap f x => Applicative (Sort f x) where
  pure x = Sort (\_ h -> (h, x)) empty
  {-# INLINE pure #-}

  (Sort f (xs :: f m x) :: Sort f x (a -> b)) <*> Sort g (ys :: f n x) =
    Sort h (merge xs ys)
    where
      h :: forall o . Proxy o -> f ((m + n) + o) x -> (f o x, b)
      h p v = case plusAssoc (size xs) (Flip ys) p of
        Refl -> case f (Proxy :: Proxy (n + o)) v of { (v', a) ->
                case g (Proxy :: Proxy o) v' of { (v'', b) ->
                  (v'', a b)}}
  {-# INLINABLE (<*>) #-}

liftSort :: IndexedHeap f x => x -> Sort f x x
liftSort a = Sort (\_ h -> case minView h of (x, h') -> (h', x)) (singleton a)
{-# INLINABLE liftSort #-}

runSort :: forall x a f. IndexedHeap f x => Sort f x a -> a
runSort (Sort f xs) =
    case addZeroZero (size xs) of
        Refl -> snd (f (Proxy :: Proxy 'Z) xs)

sortTraversable :: (IndexedHeap f a, Traversable t) => p f -> t a -> t a
sortTraversable (_ :: p f) =
    runSort .
    traverse
        (liftSort :: IndexedHeap f x =>
                     x -> Sort f x x)
{-# INLINABLE sortTraversable #-}

sortTraversal :: IndexedHeap f a => ((a -> Sort f a a) -> t -> Sort f a t) -> t -> t
sortTraversal trav = runSort . trav liftSort
{-# INLINABLE sortTraversal #-}

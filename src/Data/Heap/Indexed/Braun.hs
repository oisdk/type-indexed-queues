{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module Data.Heap.Indexed.Braun
  (Braun(..)
  ,Offset(..))
  where

import           Data.Proxy
import           Data.Type.Equality
import           GHC.TypeLits

import           Data.Heap.Indexed.Class hiding (MeldableIndexedQueue (..))

data Braun n a where
        Leaf :: Braun 0 a
        Node ::
          !(Offset n m) -> a -> Braun n a -> Braun m a -> Braun (n + m + 1) a

data Offset n m where
        Even :: Offset n n
        Lean :: Offset (1 + n) n

instance Ord a => IndexedPriorityQueue Braun a where

  insert x Leaf = Node Even x Leaf Leaf
  insert x (Node o y l r)
    | x <= y    = Node n x (insert y r) l
    | otherwise = Node n y (insert x r) l
    where n = case o of
            Even -> Lean
            Lean -> Even

  empty = Leaf

  minView (Node o x l r) = (x, merge o l r)

  minViewMay Leaf b _           = b
  minViewMay (Node o x l r) _ f = f x (merge o l r)

  singleton x = Node Even x Leaf Leaf

extract :: Ord a => Braun (n + 1) a -> (a, Braun n a)
extract (Node Even y Leaf _) = (y, Leaf)
extract (Node Even y l@Node{} r) = (x, Node Lean y r l')
  where
    (x,l') = extract l
extract (Node Lean y l r) = (x, Node Even y r l')
  where
    (x,l') = extract l

replaceMax :: Ord a => a -> Braun (n + 1) a -> Braun (n + 1) a
replaceMax x (Node o _ Leaf r) = Node o x Leaf r
replaceMax x (Node o _ l@(Node _ lx _ _) Leaf)
  | x <= lx = Node o x l Leaf
replaceMax x (Node o _ l@(Node _ lx _ _) r@(Node _ rx _ _))
  | x <= lx, x <= rx = Node o x l r
replaceMax x (Node o _ l@(Node _ lx _ _) Leaf) =
    Node o lx (replaceMax x l) Leaf
replaceMax x (Node o _ l@(Node _ lx _ _) r@(Node _ rx _ _))
  | lx <= rx = Node o lx (replaceMax x l) r
replaceMax x (Node o _ l r@(Node _ rx _ _)) = Node o rx l (replaceMax x r)

merge :: Ord a => Offset n m -> Braun n a -> Braun m a -> Braun (n + m) a
merge Even = mergeEven
merge Lean = mergeLean

mergeEven :: Ord a => Braun n a -> Braun n a -> Braun (n + n) a
mergeEven Leaf _ = Leaf
mergeEven l@(Node lo lx ll lr) r@(Node _ ly _ _)
  | lx <= ly = Node Lean lx r (merge lo ll lr)
  | otherwise =
      let (x,l') = extract l
      in Node Lean ly (replaceMax x r) l'

mergeLean :: Ord a => Braun (n + 1) a -> Braun n a -> Braun (n + n + 1) a
mergeLean l@(Node lo lx (ll :: Braun n1 a) (lr :: Braun m a)) r@(Node _ ly _ _ :: Braun n a)
  | lx > ly =
      let (x,l') = extract l
      in Node Even ly (replaceMax x r) l'
  | otherwise =
      case prf (Proxy :: Proxy n) (Proxy :: Proxy n1) (Proxy :: Proxy m) Refl of
           Refl -> Node Even lx r (merge lo ll lr)
  where
    prf
        :: forall x y z p.
           p x -> p y -> p z -> (x + 1) :~: ((y + z) + 1) -> (y + z) :~: x
    prf _ _ _ Refl = Refl
mergeLean l Leaf = l


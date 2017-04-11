{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}

module Data.BinaryTree where

import           Control.DeepSeq      (NFData(..))
import           Data.Data            (Data)
import           Data.Functor.Classes
import           Data.Monoid
import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic, Generic1)

data Tree a
    = Leaf
    | Node a
           (Tree a)
           (Tree a)
    deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable,Typeable
             ,Generic,Generic1,Data)

instance NFData a =>
         NFData (Tree a) where
    rnf Leaf = ()
    rnf (Node x l r) = rnf x `seq` rnf l `seq` rnf r `seq` ()

instance Eq1 Tree where
    liftEq _ Leaf Leaf = True
    liftEq eq (Node x xl xr) (Node y yl yr) =
        eq x y && liftEq eq xl yl && liftEq eq xr yr
    liftEq _ _ _ = False

instance Ord1 Tree where
    liftCompare _ Leaf Leaf = EQ
    liftCompare cmp (Node x xl xr) (Node y yl yr) =
        cmp x y <> liftCompare cmp xl yl <> liftCompare cmp xr yr
    liftCompare _ Leaf _ = LT
    liftCompare _ _ Leaf = GT

instance Show1 Tree where
    liftShowsPrec s _ = go where
      go _ Leaf = showString "Leaf"
      go d (Node x l r)
        = showParen (d >= 11)
        $ showString "Node "
        . s 11 x
        . showChar ' '
        . go 11 l
        . showChar ' '
        . go 11 r

instance Read1 Tree where
    liftReadsPrec r _ = go
      where
        go d ss =
            readParen
                (d > 11)
                (\xs ->
                      [ (Leaf, ys)
                      | ("Leaf",ys) <- lex xs ])
                ss ++
            readParen
                (d > 10)
                (\vs ->
                      [ (Node x lx rx, zs)
                      | ("Node",ws) <- lex vs
                      , (x,xs) <- r 11 ws
                      , (lx,ys) <- go 11 xs
                      , (rx,zs) <- go 11 ys ])
                ss

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree b f = go where
  go Leaf         = b
  go (Node x l r) = f x (go l) (go r)

unfoldTree :: (b -> Maybe (a, b, b)) -> b -> Tree a
unfoldTree f = go where
  go = maybe Leaf (\(x,l,r) -> Node x (go l) (go r)) . f

replicate :: Int -> a -> Tree a
replicate n x = go n
  where
    go m
      | m <= 0 = Leaf
      | otherwise =
          case quotRem m 2 of
              (o,1) ->
                  let r = go o
                  in Node x r r
              (e,_) -> Node x (go e) (go (e - 1))

replicateA :: Applicative f => Int -> f a -> f (Tree a)
replicateA n x = go n
  where
    go m
      | m <= 0 = pure Leaf
      | otherwise =
          case quotRem m 2 of
              (o,1) ->
                  let r = go o
                  in Node <$> x <*> r <*> r
              (e,_) -> Node <$> x <*> go e <*> go (e - 1)

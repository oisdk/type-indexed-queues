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

import           Data.Bifunctor
import           Data.Bool
import           Data.Function

-- | A simple binary tree for use in some of the heaps.
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

-- | Fold over a tree.
foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree b f = go where
  go Leaf         = b
  go (Node x l r) = f x (go l) (go r)

paraTree :: b -> (a -> Tree a -> b -> Tree a -> b -> b) -> Tree a -> b
paraTree b f = go where
  go Leaf = b
  go (Node x l r) = f x l (go l) r (go r)

zygoTree :: b1 -> (a -> b1 -> b1 -> b1) -> b -> (a -> b1 -> b -> b1 -> b -> b) -> Tree a -> b
zygoTree b1 f1 b f = snd . go where
  go Leaf = (b1,b)
  go (Node x l r) =
      let (lr1,lr) = go l
          (rr1,rr) = go r
      in (f1 x lr1 rr1, f x lr1 lr rr1 rr)

-- | Unfold a tree from a seed.
unfoldTree :: (b -> Maybe (a, b, b)) -> b -> Tree a
unfoldTree f = go where
  go = maybe Leaf (\(x,l,r) -> Node x (go l) (go r)) . f

-- | @'replicateTree' n a@ creates a tree of size @n@ filled @a@.
--
-- >>> replicateTree 4 't'
-- Node 't' (Node 't' (Node 't' Leaf Leaf) Leaf) (Node 't' Leaf Leaf)
--
-- prop> n >= 0 ==> length (replicateTree n 'a') == n
replicateTree :: Int -> a -> Tree a
replicateTree n x = go n
  where
    go m
      | m <= 0 = Leaf
      | otherwise =
          case quotRem m 2 of
              (o,1) ->
                  let r = go o
                  in Node x r r
              (e,_) -> Node x (go e) (go (e - 1))

-- | @'replicateA' n a@ replicates the action @a@ @n@ times.
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

instance Monoid (Tree a) where
    mappend Leaf y = y
    mappend (Node x l r) y = Node x l (mappend r y)
    mempty = Leaf

treeFromList :: [a] -> Tree a
treeFromList [] = Leaf
treeFromList (x:xs) = uncurry (Node x `on` treeFromList) (pairs xs) where
  pairs ys = foldr f (const ([],[])) ys True
  f e a b = bool first second b (e:) (a (not b))

treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold _ b [] = b
treeFold _ _ [x] = x
treeFold f b (x:y:xs) = treeFold f b (f x y : pairfold xs)
    where
        pairfold (xx:yy:rest) = f xx yy : pairfold rest
        pairfold l = l

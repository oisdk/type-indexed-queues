module Data.Heap.Braun where

import Data.BinaryTree

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y l r)
    | x <= y    = Node x (insert y r) l
    | otherwise = Node y (insert x r) l

extract :: Ord a => Tree a -> (a, Tree a)
extract (Node y Leaf Leaf) = (y, Leaf)
extract (Node y l r) = let (x,l') = extract l in (x, Node y r l')
extract Leaf = error "extract called on empty braun tree"

proper :: Ord a => Tree a -> Bool
proper Leaf = True
proper (Node _ Leaf r) = case r of
  Leaf -> True
  _     -> False
proper (Node x (Node y _ Leaf) Leaf) = x <= y
proper (Node x l@(Node lx _ _) r@(Node rx _ _))
  = ((length r <= length l) && (length l <= (length r + 1)))
  && x <= lx && x <= rx && proper l && proper r
proper _ = False

merge :: Ord a => Tree a -> Tree a -> Tree a
merge xs Leaf = xs
merge Leaf ys = ys
merge l@(Node lx ll lr) r@(Node ly _ _)
  | lx <= ly = Node lx r (merge ll lr)
  | otherwise = let (x,l') = extract l in
    Node ly (replaceMin x r) l'

replaceMin :: Ord a => a -> Tree a -> Tree a
replaceMin x (Node _ l r)
  | isAbove x l, isAbove x r = Node x l r
replaceMin x (Node _ l@(Node lx _ _) r)
  | isAbove lx r = Node lx (replaceMin x l) r
replaceMin x (Node l _ r@(Node _ rx _)) = Node l rx (replaceMin x r)
replaceMin _ x = x

isAbove :: Ord a => a -> Tree a -> Bool
isAbove _ Leaf        = True
isAbove x (Node y _ _) = x <= y

minView :: Ord a => Tree a -> Maybe (a, Tree a)
minView Leaf        = Nothing
minView (Node x l r) = Just (x, merge l r)

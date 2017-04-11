{-# LANGUAGE DeriveFoldable #-}

module Data.Heap.Braun where

data Braun a = Empty | Node (Braun a) a (Braun a) deriving Foldable

insert :: Ord a => a -> Braun a -> Braun a
insert x Empty = Node Empty x Empty
insert x (Node l y r)
    | x <= y    = Node (insert y r) x l
    | otherwise = Node (insert x r) y l

extract :: Ord a => Braun a -> (a, Braun a)
extract (Node Empty y Empty) = (y, Empty)
extract (Node l y r) = let (x,l') = extract l in (x, Node r y l')
extract Empty = error "extract called on empty braun tree"

proper :: Ord a => Braun a -> Bool
proper Empty = True
proper (Node Empty _ r) = case r of
  Empty -> True
  _     -> False
proper (Node (Node Empty y Empty) x Empty) = x <= y
proper (Node l@(Node _ lx _) x r@(Node _ rx _))
  = ((length r <= length l) && (length l <= (length r + 1)))
  && x <= lx && x <= rx && proper l && proper r
proper _ = False

merge :: Ord a => Braun a -> Braun a -> Braun a
merge xs Empty = xs
merge Empty ys = ys
merge l@(Node ll lx lr) r@(Node _ ly _)
  | lx <= ly = Node r lx (merge ll lr)
  | otherwise = let (x,l') = extract l in
    Node (replaceMin x r) ly l'

replaceMin :: Ord a => a -> Braun a -> Braun a
replaceMin x (Node l _ r)
  | isAbove x l, isAbove x r = Node l x r
replaceMin x (Node l@(Node _ lx _) _ r)
  | isAbove lx r = Node (replaceMin x l) lx r
replaceMin x (Node l _ r@(Node _ rx _)) = Node l rx (replaceMin x r)
replaceMin _ x = x

isAbove :: Ord a => a -> Braun a -> Bool
isAbove _ Empty        = True
isAbove x (Node _ y _) = x <= y

minView :: Ord a => Braun a -> Maybe (a, Braun a)
minView Empty        = Nothing
minView (Node l x r) = Just (x, merge l r)

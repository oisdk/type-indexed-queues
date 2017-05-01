-- | Functions for creating rose trees (from "Data.Tree") of a specified
-- size.
module Data.Tree.Replicate where

import           Control.Monad
import           Data.Tree

import           Data.Functor.Identity

-- | @'replicateA' n x@ replicates the action @x@ @n@ times.
replicateA :: Applicative f => Int -> f a -> f (Tree a)
replicateA t x = go t
  where
    go n = Node <$> x <*> goList (n - 1)
    goList 0 = pure []
    goList n =
        let m = ceiling (sqrt (toEnum n :: Double))
        in case quotRem n m of
               (lm,0) -> replicateM lm (go m)
               (lm,r) -> (:) <$> go r <*> replicateM lm (go m)
{-# SPECIALIZE replicateA :: Int -> Identity a -> Identity (Tree a) #-}

-- | @'replicateTree' n a@ creates a tree of size @n@ filled with @a@.
--
-- >>> putStr (drawTree (replicateTree 4 "."))
-- .
-- |
-- +- .
-- |
-- `- .
--    |
--    `- .
--
-- prop> n > 0 ==> length (replicateTree n x) == n
replicateTree :: Int -> a -> Tree a
replicateTree t x = runIdentity (replicateA t (Identity x))

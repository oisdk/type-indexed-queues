-- | Functions for creating rose trees (from "Data.Tree") of a specified
-- size.
module Data.Tree.Replicate where

import           Control.Monad
import           Data.Tree

-- | @'replicateA' n x@ replicates the action @x@ @n@ times.
replicateA :: Applicative f => Int -> f a -> f (Tree a)
replicateA t x = go t
  where
    go n
      | n <= 1 = flip Node [] <$> x
    go n =
        let m =
                head
                    [ y
                    | y <- [1 ..]
                    , y * y >= (n - 1) ]
            lm = (n - 1) `div` m
        in if m * lm == (n - 1)
               then Node <$> x <*> replicateM lm (go m)
               else Node <$> x <*>
                    ((:) <$> go ((n - 1) - m * lm) <*> replicateM lm (go m))

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
replicateTree t x = go t where
 go n | n <= 1 = Node x []
 go n =
   let m = head [ y | y <- [1..], y * y >= (n-1) ]
       lm = (n-1) `div` m
   in if m * lm == (n-1)
      then Node x (replicate lm (go m))
      else Node x (go ((n-1) - m * lm) : replicate lm (go m))

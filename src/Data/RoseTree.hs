{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}

module Data.RoseTree where

import           Control.DeepSeq      (NFData (..))
import           Control.Monad
import           Data.Data            (Data)
import           Data.Functor.Classes
import           Data.Monoid
import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic, Generic1)

-- | A simple rose tree for use in some of the heaps.
data Tree a =
    Root a [Tree a]
    deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable
             ,Typeable,Generic,Generic1,Data)

instance NFData a => NFData (Tree a) where
    rnf (Root r xs) = rnf r `seq` rnf xs `seq` ()

instance Eq1 Tree where
    liftEq (===) = (=~=)
      where
        Root x xs =~= Root y ys = x === y && liftEq (=~=) xs ys

instance Ord1 Tree where
    liftCompare cmp = cmpTree
      where
        cmpTree (Root x xs) (Root y ys) = cmp x y <> liftCompare cmpTree xs ys

instance Show1 Tree where
    liftShowsPrec s l = go where
      go d (Root x xs)
          = showParen (d >= 11)
          $ showString "Root "
          . s 11 x
          . showChar ' '
          . liftShowsPrec go (liftShowList s l) 11 xs

-- | @'replicateA' n x@ replicates the action @x@ @n@ times.
replicateA :: Applicative f => Int -> f a -> f (Tree a)
replicateA t x = go t
  where
    go n
      | n <= 1 = flip Root [] <$> x
    go n =
        let m =
                head
                    [ y
                    | y <- [1 ..]
                    , y * y >= (n - 1) ]
            lm = (n - 1) `div` m
        in if m * lm == (n - 1)
               then Root <$> x <*> replicateM lm (go m)
               else Root <$> x <*>
                    ((:) <$> go ((n - 1) - m * lm) <*> replicateM lm (go m))

-- | @'replicateTree' n a@ creates a tree of size @n@ filled with @a@.
--
-- >>> replicateTree 4 'a'
-- Root 'a' [Root 'a' [],Root 'a' [Root 'a' []]]
--
-- prop> n > 0 ==> length (replicateTree n 'a') == n
replicateTree :: Int -> a -> Tree a
replicateTree t x = go t where
 go n | n <= 1 = Root x []
 go n =
   let m = head [ y | y <- [1..], y * y >= (n-1) ]
       lm = (n-1) `div` m
   in if m * lm == (n-1)
      then Root x (replicate lm (go m))
      else Root x (go ((n-1) - m * lm) : replicate lm (go m))

instance Read1 Tree where
    liftReadsPrec r l = go
      where
        go d =
            readParen
                (d > 10)
                (\ws ->
                      [ (Root x lx, zs)
                      | ("Root",xs) <- lex ws
                      , (x,ys) <- r 11 xs
                      , (lx,zs) <- liftReadsPrec go (liftReadList r l) 11 ys ])

instance Applicative Tree where
    pure = flip Root []
    (<*>) = ap

instance Monad Tree where
    Root x ts >>= f = Root x' (ts' ++ map (>>= f) ts)
      where Root x' ts' = f x

-- | Fold over the items in a tree.
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Root x ts) = f x (map go ts)

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Main (main) where

import           Test.DocTest
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.BinaryTree
import qualified Data.RoseTree              as Rose

import           Data.Heap.Binomial         hiding (Tree)
import qualified Data.Heap.Binomial         as Binomial
import           Data.Heap.Braun            (Braun (..))
import           Data.Heap.Leftist          (Leftist)
import           Data.Heap.Pairing          (Pairing)
import           Data.Heap.Skew

import qualified Data.Heap.Indexed.Binomial as Indexed
import qualified Data.Heap.Indexed.Braun    as Indexed
import           Data.Heap.Indexed.Erased
import qualified Data.Heap.Indexed.Leftist  as Indexed
import qualified Data.Heap.Indexed.Pairing  as Indexed
import qualified Data.Heap.Indexed.Skew     as Indexed

import           Data.Heap.Class
import           Data.Heap.Indexed.Class

import           TypeLevel.Nat

import           Data.List                  (sort)

import           Data.Proxy

import           Data.Functor.Classes

properBinomial :: Ord a => Binomial 'Z a -> Bool
properBinomial = go 1 where
  go :: forall z a. Ord a => Int -> Binomial z a -> Bool
  go _ Nil       = True
  go n (Skip xs) = go (n * 2) xs
  go n (x :- xs) = length x == n && properTree x && go (n * 2) xs

  properTree :: forall z a. Ord a => Binomial.Tree z a -> Bool
  properTree (Root x xs) = all (>=x) xs && properNode xs

  properNode :: forall z a. Ord a => Node z a -> Bool
  properNode (t :< ts) = properTree t && properNode ts
  properNode NilN      = True

fromList' :: (PriorityQueue h Int) => [Int] -> h Int
fromList' = fromList

propHeapSort :: (PriorityQueue h Int) => p h -> TestTree
propHeapSort p =
    testProperty "sort" $
    \xs ->
         heapSort p (xs :: [Int]) === sort xs

properBraun :: Ord a => Braun a -> Bool
properBraun (Braun Leaf) = True
properBraun (Braun (Node x l r)) =
    length r <= length l &&
    length l <= length r + 1 &&
    all (x <=) l &&
    all (x <=) r && properBraun (Braun l) && properBraun (Braun r)

indexedSort :: IndexedPriorityQueue h Int => Proxy h -> TestTree
indexedSort (_ :: Proxy h) =
    testProperty
        "sort"
        (\xs ->
              heapSort (Proxy :: Proxy (ErasedSize h)) (xs :: [Int]) ===
              sort xs)

intTree :: Gen (Tree Int)
intTree = sized (`replicateA` arbitrary)

intRoseTree :: Gen (Rose.Tree Int)
intRoseTree = sized (`Rose.replicateA` arbitrary)

reflexiveEq :: (Eq a, Show a) => Gen a -> Property
reflexiveEq xs = forAll xs (\x -> x === x)

symmetricEq :: (Eq a, Show a) => Gen a -> Property
symmetricEq xs =
    forAll xs $
    \x ->
         forAll xs $
         \y ->
              let e = x == y
              in collect e $ e == (y == x)

{-# ANN equalityProps "HLint: ignore Use ==" #-}
equalityProps :: (Eq a, Show a) => Gen a -> TestTree
equalityProps xs =
    testGroup
        "equality"
        [ testProperty "reflexive" (reflexiveEq xs)
        , testProperty "symmetric" (symmetricEq xs)
        , testProperty
              "correct /="
              (forAll xs $
               \x ->
                    forAll xs $
                    \y ->
                         let e = x == y
                         in collect e $ e === not (x /= y))]

readShow :: (Eq a, Read a, Show a) => Gen a -> TestTree
readShow xs =
    testProperty "read . show" $
    forAll xs $
    \x ->
         (read . show) x === x

-- | Test that manual Read1 / Show1 classes are equivalent to derived read/show.
readShow1
    :: (Read1 f, Show1 f, Show (f a), Show a, Read a, Read (f a), Eq (f a))
    => Gen (f a) -> TestTree
readShow1 (xs :: Gen (f a)) =
    testGroup
        "readshow1"
        [ testProperty "show1" $
          forAll xs $
          \x ->
               manualShow x === show x
        , testProperty "read1 . show1" $
          forAll xs $
          \x ->
               forAll arbitrary $
               \n ->
                    (liftReadsPrec readsPrec readList n . manualShow) x ===
                    ((readsPrec n . show) x :: [(f a, String)])]
  where
    manualShow x = liftShowsPrec showsPrec showList 0 x ""

liftedEq :: (Eq1 f, Show (f a), Eq a, Eq (f a)) => Gen (f a) -> TestTree
liftedEq xs =
    testProperty "eq1" $
    forAll xs $
    \x ->
         forAll xs $
         \y ->
              let e = (x == y)
              in collect e $ liftEq (==) x y == e

{-# ANN ordProps "HLint: ignore Use ==" #-}
ordProps :: (Ord a, Show a) => Gen a -> TestTree
ordProps xs =
    testGroup
        "ordering"
        [ testProperty "reflexive ord" $
          forAll xs $
          \x ->
               compare x x === EQ
        , testProperty "symmetric ord" $
          forAll xs $
          \x ->
               forAll xs $
               \y ->
                    let c = compare x y
                    in collect c $ c === inv (compare y x)
        , testProperty "same as ==" $
          forAll xs $
          \x ->
               forAll xs $
               \y ->
                    let c = compare x y
                    in collect c $ (c == EQ) === (x == y)]
  where
    inv EQ = EQ
    inv LT = GT
    inv GT = LT

liftedOrd :: (Ord1 f, Show (f a), Ord a, Ord (f a)) => Gen (f a) -> TestTree
liftedOrd xs =
    testProperty "compare1" $
    forAll xs $
    \x ->
         forAll xs $
         \y ->
              let c = compare x y
              in collect c $ c === liftCompare compare x y

main :: IO ()
main = do
    doctest ["-isrc", "src"]
    defaultMain $
        testGroup
            "Tests"
            [ testGroup
                  "Binomial"
                  [ testProperty "proper" (properBinomial . fromList')
                  , propHeapSort (Proxy :: Proxy (Binomial 'Z))]
            , testGroup
                  "Braun"
                  [ testProperty "proper" (properBraun . fromList :: [Int] -> Bool)
                  , propHeapSort (Proxy :: Proxy Braun) ]
            , testGroup
                  "Leftist"
                  [ propHeapSort (Proxy :: Proxy Leftist) ]
            , testGroup
                  "Pairing"
                  [ propHeapSort (Proxy :: Proxy Pairing) ]
            , testGroup
                  "Skew"
                  [ propHeapSort (Proxy :: Proxy Skew) ]
            , testGroup
                  "Indexed Braun"
                  [ indexedSort (Proxy :: Proxy Indexed.Braun) ]
            , testGroup
                  "Indexed Binomial"
                  [ indexedSort (Proxy :: Proxy (Indexed.Binomial 0)) ]
            , testGroup
                  "Indexed Leftist"
                  [ indexedSort (Proxy :: Proxy Indexed.Leftist) ]
            , testGroup
                  "Indexed Pairing"
                  [ indexedSort (Proxy :: Proxy Indexed.Pairing) ]
            , testGroup
                  "Indexed Skew"
                  [ indexedSort (Proxy :: Proxy Indexed.Skew) ]
            , testGroup
                  "Binary Tree"
                  [ readShow intTree
                  , readShow1 intTree
                  , equalityProps intTree
                  , liftedEq intTree
                  , ordProps intTree
                  , liftedOrd intTree ]
            , testGroup
                  "Rose Tree"
                  [ readShow intRoseTree
                  , readShow1 intRoseTree
                  , equalityProps intRoseTree
                  , liftedEq intTree
                  , ordProps intTree
                  , liftedOrd intTree ]
            ]

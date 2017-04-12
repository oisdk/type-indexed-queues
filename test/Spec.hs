{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Test.DocTest
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.BinaryTree
import           Data.Heap.Binomial         hiding (Tree)
import qualified Data.Heap.Binomial         as Binomial
import           Data.Heap.Braun            (Braun (..))
import           Data.Heap.Pairing          (Pairing)
import           Data.Heap.Leftist          (Leftist)
import           Data.Heap.Skew

import qualified Data.Heap.Indexed.Binomial as Indexed
import qualified Data.Heap.Indexed.Braun    as Indexed
import qualified Data.Heap.Indexed.Leftist  as Indexed
import qualified Data.Heap.Indexed.Pairing  as Indexed
import qualified Data.Heap.Indexed.Skew     as Indexed
import           Data.Heap.Indexed.Erased

import           Data.Heap.Class
import           Data.Heap.Indexed.Class

import           TypeLevel.Nat

import           Data.List                  (sort)

import           Data.Proxy

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

fromList' :: PriorityQueue h => [Int] -> h Int
fromList' = fromList

propHeapSort :: PriorityQueue h => p h -> TestTree
propHeapSort p =
    testProperty "sort" $
    \xs ->
         heapSort p (xs :: [Int]) === sort xs

properBraun :: Ord a => Braun a -> Bool
properBraun (Braun Leaf) = True
properBraun (Braun (Node x l r)) =
    length r <= length l &&
    length l <= length r + 1 &&
    all (x <=) l && all (x <=) r && properBraun (Braun l) && properBraun (Braun r)

indexedSort :: IndexedPriorityQueue h => Proxy h -> TestTree
indexedSort (_ :: Proxy h) =
    testProperty
        "sort"
        (\xs ->
              heapSort (Proxy :: Proxy (ErasedSize h)) (xs :: [Int]) ===
              sort xs)

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
                  "Tree"
                  [ testProperty "readshow" $
                    forAll (sized $ flip replicateA arbitrary) $
                    \xs ->
                         (read . show) xs === (xs :: Tree Int)]]

{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import           Test.DocTest
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.BinaryTree
import           Data.Heap.Binomial    hiding (Tree)
import qualified Data.Heap.Binomial    as Binomial
import           Data.Heap.Braun       (Braun(..))
-- import qualified Data.Heap.Braun       as Braun
-- import qualified Data.Heap.Leftist     as Leftist
-- import           Data.Heap.Pairing
-- import           Data.Heap.Skew

import qualified Data.Heap.Indexed.Braun as Indexed

import           Data.Heap.Class

import           TypeLevel.Nat

import           Data.List             (sort)

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

main :: IO ()
main = do
    doctest ["-isrc", "src"]
    defaultMain $
        testGroup
            "Tests"
            [ testGroup
                  "Binomial heap"
                  [ testProperty "proper" (properBinomial . fromList')
                  , propHeapSort (Proxy :: Proxy (Binomial 'Z))]
            , testGroup
                  "Braun"
                  [ testProperty
                        "proper"
                        (properBraun . fromList :: [Int] -> Bool)
                  , testProperty
                        "sort"
                        (\xs ->
                              heapSort (Proxy :: Proxy Braun)
                                  (xs :: [Int]) ===
                              sort xs)]
            -- , testGroup
            --       "Indexed Braun"
            --       [ testProperty
            --             "sort"
            --             (\xs ->
            --                   Indexed.braunSort (xs :: [Int]) === sort xs)]
            , testGroup
                  "Tree"
                  [ testProperty "readshow" $
                    forAll (sized $ flip replicateA arbitrary) $
                    \xs ->
                         (read . show) xs === (xs :: Tree Int)]]

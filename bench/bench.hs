{-# LANGUAGE DataKinds #-}

import           Criterion.Main
import           System.Random

import           Data.Traversable.Sort
import           Data.Heap.Class

import qualified Data.Heap.Indexed.Binomial as Indexed
import qualified Data.Heap.Indexed.Pairing  as Indexed
import qualified Data.Heap.Indexed.Skew     as Indexed
import qualified Data.Heap.Indexed.Leftist  as Indexed

import           Data.Heap.Binomial
import           Data.Heap.Pairing
import           Data.Heap.Skew
import           Data.Heap.Leftist

import           Control.Monad              (replicateM)

import           Data.List                  (sort)
import qualified Data.Sequence              as Seq
import qualified Data.PQueue.Min            as P

import           Data.Proxy

import           TypeLevel.Nat

randInt :: IO Int
randInt = randomIO

testSize :: Int -> Benchmark
testSize n =
    bgroup
        (show n)
        [ env (Seq.replicateM n randInt) $
          \xs ->
               bgroup
                   "seq"
                   [ bench "trav binom"       $ nf (sortTraversable (Proxy :: Proxy (Indexed.Binomial 0))) xs
                   , bench "trav pairing"     $ nf (sortTraversable (Proxy :: Proxy Indexed.Pairing)) xs
                   , bench "trav skew"        $ nf (sortTraversable (Proxy :: Proxy Indexed.Skew)) xs
                   , bench "trav leftist"     $ nf (sortTraversable (Proxy :: Proxy Indexed.Leftist)) xs
                   , bench "Seq.sort"         $ nf Seq.sort xs
                   , bench "Seq.unstableSort" $ nf Seq.unstableSort xs
                   ]
        , env (replicateM n randInt) $
          \xs ->
               bgroup
                   "list"
                   [ bench "Data.List"    $ nf sort xs
                   , bench "sort pairing" $ nf (heapSort (Proxy :: Proxy Pairing)) xs
                   , bench "sort leftist" $ nf (heapSort (Proxy :: Proxy Leftist)) xs
                   , bench "sort binom"   $ nf (heapSort (Proxy :: Proxy (Binomial 'Z))) xs
                   , bench "sort skew"    $ nf (heapSort (Proxy :: Proxy Skew)) xs
                   , bench "sort pqueue"  $ nf (P.toList . P.fromList) xs
                   , bench "trav pairing" $ nf (sortTraversable (Proxy :: Proxy Indexed.Pairing)) xs
                   , bench "trav leftist" $ nf (sortTraversable (Proxy :: Proxy Indexed.Leftist)) xs
                   , bench "trav binom"   $ nf (sortTraversable (Proxy :: Proxy (Indexed.Binomial 0))) xs
                   , bench "trav skew"    $ nf (sortTraversable (Proxy :: Proxy Indexed.Skew)) xs
                   ]
        ]

main :: IO ()
main =
    defaultMain $
    map
        testSize
        [500, 1000, 10000, 100000, 500000]

{-# LANGUAGE DataKinds #-}

import           Criterion.Main
import           System.Random

import           Data.Traversable.Parts
import           Data.Queue.Class

import qualified Data.Queue.Indexed.Binomial as Indexed
import qualified Data.Queue.Indexed.Pairing  as Indexed
import qualified Data.Queue.Indexed.Skew     as Indexed
import qualified Data.Queue.Indexed.Leftist  as Indexed
import qualified Data.Queue.Indexed.Braun    as Indexed
import qualified Data.Queue.Indexed.Splay    as Indexed

import           Data.Queue.Indexed.Erased

import           Data.Queue.Binomial
import           Data.Queue.Pairing
import           Data.Queue.Skew
import           Data.Queue.Leftist

import           Control.Monad              (replicateM)

import           Data.List                  (sort)
import qualified Data.Sequence              as Seq
import qualified Data.PQueue.Min            as P

import           Data.Proxy

import           TypeLevel.Nat

import           Data.BinaryTree

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
                   [ bench "trav splay"       $ nf (queueTraversable (Proxy :: Proxy Indexed.Splay)) xs
                   , bench "trav binom"       $ nf (queueTraversable (Proxy :: Proxy (Indexed.Binomial 0))) xs
                   , bench "trav pairing"     $ nf (queueTraversable (Proxy :: Proxy Indexed.Pairing)) xs
                   , bench "trav skew"        $ nf (queueTraversable (Proxy :: Proxy Indexed.Skew)) xs
                   , bench "trav leftist"     $ nf (queueTraversable (Proxy :: Proxy Indexed.Leftist)) xs
                   , bench "Seq.sort"         $ nf Seq.sort xs
                   , bench "Seq.unstableSort" $ nf Seq.unstableSort xs
                   ]
        , env (replicateA n randInt) $
          \xs ->
               bgroup
                   "tree"
                   [ bench "trav binom"       $ nf (queueTraversable (Proxy :: Proxy (Indexed.Binomial 0))) xs
                   , bench "trav pairing"     $ nf (queueTraversable (Proxy :: Proxy Indexed.Pairing)) xs
                   , bench "trav skew"        $ nf (queueTraversable (Proxy :: Proxy Indexed.Skew)) xs
                   , bench "trav leftist"     $ nf (queueTraversable (Proxy :: Proxy Indexed.Leftist)) xs
                   ]
        , env (replicateM n randInt) $
          \xs ->
               bgroup
                   "list"
                   [ bench "sort braun"   $ nf (heapSort (Proxy :: Proxy (ErasedSize Indexed.Braun))) xs
                   , bench "Data.List"    $ nf sort xs
                   , bench "sort pairing" $ nf (heapSort (Proxy :: Proxy Pairing)) xs
                   , bench "sort leftist" $ nf (heapSort (Proxy :: Proxy Leftist)) xs
                   , bench "sort binom"   $ nf (heapSort (Proxy :: Proxy (Binomial 'Z))) xs
                   , bench "sort skew"    $ nf (heapSort (Proxy :: Proxy Skew)) xs
                   , bench "sort pqueue"  $ nf (P.toList . P.fromList) xs
                   , bench "trav pairing" $ nf (queueTraversable (Proxy :: Proxy Indexed.Pairing)) xs
                   , bench "trav leftist" $ nf (queueTraversable (Proxy :: Proxy Indexed.Leftist)) xs
                   , bench "trav binom"   $ nf (queueTraversable (Proxy :: Proxy (Indexed.Binomial 0))) xs
                   , bench "trav skew"    $ nf (queueTraversable (Proxy :: Proxy Indexed.Skew)) xs
                   ]
        ]

main :: IO ()
main =
    defaultMain $
    map
        testSize
        [500, 1000, 10000, 100000, 500000]

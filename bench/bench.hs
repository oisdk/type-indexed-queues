import           Criterion.Main
import           System.Random

import           Data.Traversable.Sort
import           Data.Heap.Skew.Indexed

import           Control.Monad     (replicateM)

import Data.List (sort)

-- import qualified Data.PQueue.Min as P

import Data.Proxy

randInt :: IO Int
randInt = randomIO

testSize :: Int -> Benchmark
testSize n =
    env (replicateM n randInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "mn" $ nf (sortTraversable (Proxy :: Proxy Heap)) xs
             , bench "nmn" $ nf sort xs]

main :: IO ()
main =
    defaultMain $
    map
        testSize
        [500, 1000, 10000]

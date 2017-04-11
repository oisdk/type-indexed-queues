import           Criterion.Main
import           System.Random

import           Data.BinomialHeap
import           Data.BinomialHeap.Indexed

import           Control.Monad     (replicateM)

import Data.List (sort)

import qualified Data.PQueue.Min as P

randInt :: IO Int
randInt = randomIO

testSize :: Int -> Benchmark
testSize n =
    env (replicateM n randInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "mn" $ nf (toList . fromList) xs
             , bench "nmn" $ nf (P.toList . P.fromList) xs]

main :: IO ()
main =
    defaultMain $
    map
        testSize
        [100000, 100000, 100000]

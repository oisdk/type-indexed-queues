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

import           Data.Queue.Binomial         hiding (Tree)
import qualified Data.Queue.Binomial         as Binomial
import           Data.Queue.Braun            (Braun (..))
import           Data.Queue.Leftist          (Leftist,zygoLeftist)
import           Data.Queue.Pairing          (Pairing(..))
import           Data.Queue.Skew

import qualified Data.Queue.Indexed.Binomial as Indexed
import qualified Data.Queue.Indexed.Braun    as Indexed
import           Data.Queue.Indexed.Erased
import qualified Data.Queue.Indexed.Leftist  as Indexed
import qualified Data.Queue.Indexed.Pairing  as Indexed
import qualified Data.Queue.Indexed.Skew     as Indexed

import           Data.Queue.Class
import           Data.Queue.Indexed.Class

import           TypeLevel.Nat

import           Data.List                  (sort)
import           Data.Monoid

import           Data.Proxy

import           Data.Functor.Classes


binomial :: Ord a => Binomial 'Z a -> Bool
binomial = go 1 where
  go :: forall z a. Ord a => Int -> Binomial z a -> Bool
  go _ Nil       = True
  go n (Skip xs) = go (n * 2) xs
  go n (x :- xs) = length x == n && properTree x && go (n * 2) xs

  properTree :: forall z a. Ord a => Binomial.Tree z a -> Bool
  properTree (Root x xs) = isAbove x xs && properNode xs

  properNode :: forall z a. Ord a => Node z a -> Bool
  properNode (t :< ts) = properTree t && properNode ts
  properNode NilN      = True

pairing :: Ord a => Pairing a -> Bool
pairing E = True
pairing (T x xs) = all (\y -> isAbove x y && pairing y) xs

skew :: Ord a => Skew a -> Bool
skew (Skew xs) = isHeap xs

lengthAlg :: (Int, a -> Int -> Int -> Int)
lengthAlg = (0, const (+))

isAbove :: (Ord a, Foldable f) => a -> f a -> Bool
isAbove x = all (x<=)

propHeapSort :: (Queue h Int) => Proxy h -> TestTree
propHeapSort p =
    testProperty "sort" $
    \xs ->
         heapSort p (xs :: [Int]) === sort xs

braun :: Ord a => Braun a -> Bool
braun (Braun xs) = isHeap xs && uncurry zygoTree lengthAlg True go xs where
  go _ llen lproper rlen rproper =
    rlen <= llen &&
    llen <= rlen + 1 &&
    lproper && rproper

indexedSort :: IndexedQueue h Int => Proxy h -> TestTree
indexedSort (_ :: Proxy h) =
    testProperty
        "sort"
        (\xs ->
              heapSort (Proxy :: Proxy (ErasedSize h)) (xs :: [Int]) ===
              sort xs)

leftist :: Ord a => Leftist a -> Bool
leftist =
    zygoLeftist
        (Nothing, 0)
        (\_ x (_,ls) (_,rs) ->
              (Just x, succ (ls + rs)))
        True
        go
  where
    go i x (lval,ls) lproper (rval,rs) rproper =
        isAbove x lval &&
        isAbove x rval &&
        lproper && rproper && i == succ (ls + rs) && rs <= ls

intTree :: Gen (Tree Int)
intTree = sized (`replicateA` arbitrary)

reflexiveEq :: (Eq a, Show a) => Gen a -> Property
reflexiveEq xs = forAll xs (\x -> x === x)

eqProp :: (Eq a) => (a -> a -> Bool) -> Gen a -> Gen Property
eqProp f xs = do
    x <- xs
    y <- xs
    let e = x == y
    pure $ collect e $ e === f x y

symmetricEq :: (Eq a, Show a) => Gen a -> Gen Property
symmetricEq = eqProp (flip (==))

{-# ANN equalityProps "HLint: ignore Use ==" #-}
equalityProps :: (Eq a, Show a) => Gen a -> TestTree
equalityProps xs =
    testGroup
        "equality"
        [ testProperty "reflexive" (reflexiveEq xs)
        , testProperty "symmetric" (symmetricEq xs)
        , testProperty "correct /=" (eqProp (\x y -> not (x /= y)) xs)]

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
          do x <- xs
             n <- arbitrary
             pure $
                 (liftReadsPrec readsPrec readList n . manualShow) x ===
                 ((readsPrec n . show) x :: [(f a, String)])]
  where
    manualShow x = liftShowsPrec showsPrec showList 0 x ""

liftedEq :: (Eq1 f, Show (f a), Eq a, Eq (f a)) => Gen (f a) -> TestTree
liftedEq = testProperty "eq1" . eqProp (liftEq (==))

{-# ANN ordProps "HLint: ignore Use ==" #-}
ordProps :: (Ord a, Show a) => Gen a -> TestTree
ordProps xs =
    testGroup
        "ordering"
        [ testProperty "reflexive ord" $
          forAll xs $
          \x ->
               compare x x === EQ
        , testProperty "symmetric ord" $ cmpProp (\x y -> inv (compare y x)) xs
        , testProperty "same as ==" $ eqProp (\x y -> compare x y == EQ) xs]
  where
    inv EQ = EQ
    inv LT = GT
    inv GT = LT

cmpProp :: (Ord a, Show a) => (a -> a -> Ordering) -> Gen a -> Gen Property
cmpProp f xs = do
    x <- xs
    y <- xs
    let c = compare x y
    pure $ collect c $ c === f x y

monoidProps :: (Monoid a, Show a, Eq a) => Gen a -> TestTree
monoidProps xs =
    testGroup
        "monoid"
        [ testProperty "associativity" $
          do x <- xs
             y <- xs
             z <- xs
             pure $ (x <> y) <> z === x <> (y <> z)
        , testProperty "left identity" $
          do x <- xs
             pure $ x === mempty <> x
        , testProperty "right identity" $
          do x <- xs
             pure $ x === x <> mempty]

liftedOrd :: (Ord1 f, Show (f a), Ord a, Ord (f a)) => Gen (f a) -> TestTree
liftedOrd =
    testProperty "compare1" . cmpProp (liftCompare compare)

{-# ANN fmapLaw "HLint: ignore Functor law" #-}
fmapLaw
    :: (Functor f, Eq (f a), Show (f a))
    => Gen (f a) -> Property
fmapLaw xs = forAll xs $ \x -> fmap id x === x

{-# ANN fmapCompLaw "HLint: ignore Functor law" #-}
fmapCompLaw
    :: (Functor f, Eq (f c), Show (f c))
    => Blind (b -> c) -> Blind (a -> b) -> f a -> Property
fmapCompLaw (Blind f) (Blind g) xs =
 fmap (f . g) xs === (fmap f . fmap g) xs

functorLaws
    :: (Functor f
       ,Show (f a)
       ,Eq (f a)
       ,Eq (f c)
       ,Show (f c)
       ,CoArbitrary b
       ,Arbitrary c
       ,CoArbitrary a
       ,Arbitrary b)
    => p b -> q c -> Gen (f a) -> TestTree
functorLaws (_ :: p b) (_ :: q c) (xs :: Gen (f a)) =
    testGroup
        "functor laws"
        [ testProperty "identity" (fmapLaw xs)
        , testProperty
              "composition"
              (fmapCompLaw <$> (arbitrary :: Gen (Blind (b -> c))) <*>
               (arbitrary :: Gen (Blind (a -> b))) <*>
               xs)]

withGen :: Functor f => a -> f (a -> b) -> f b
withGen = fmap . flip ($)

proper :: Show a => (a -> Bool) -> Gen a -> TestTree
proper p xs = testProperty "proper" $ do
    x <- xs
    pure $ counterexample (show x) (p x)

main :: IO ()
main = do
    doctest ["-isrc", "src"]
    defaultMain $
        testGroup
            "Tests"
            [ let xs = fmap (fromList :: [Int] -> Binomial 'Z Int) arbitrary
              in testGroup
                     "Binomial"
                     [ proper binomial xs
                     , propHeapSort (Proxy :: Proxy (Binomial 'Z))
                     , readShow xs
                     , equalityProps xs
                     , ordProps xs
                     , monoidProps xs
                     , functorLaws (Proxy :: Proxy Int) (Proxy :: Proxy Int) xs]
            , let xs = fmap (fromList :: [Int] -> Braun Int) arbitrary
              in testGroup
                     "Braun"
                     [ proper braun xs
                     , propHeapSort (Proxy :: Proxy Braun)
                     , readShow xs
                     , equalityProps xs
                     , ordProps xs
                     , functorLaws (Proxy :: Proxy Int) (Proxy :: Proxy Int) xs]
            , testGroup "Leftist" $
              propHeapSort (Proxy :: Proxy Leftist) :
              withGen
                  (fmap (fromList :: [Int] -> Leftist Int) arbitrary)
                  [ proper leftist
                  , readShow
                  , equalityProps
                  , ordProps
                  , monoidProps
                  , functorLaws (Proxy :: Proxy Int) (Proxy :: Proxy Int)]
            , testGroup "Pairing" $
              propHeapSort (Proxy :: Proxy Pairing) :
              withGen
                  (fmap (fromList :: [Int] -> Pairing Int) arbitrary)
                  [ proper pairing
                  , readShow
                  , equalityProps
                  , ordProps
                  , monoidProps
                  , functorLaws (Proxy :: Proxy Int) (Proxy :: Proxy Int)]
            , testGroup "Skew" $
              propHeapSort (Proxy :: Proxy Skew) :
              withGen
                  (fmap (fromList :: [Int] -> Skew Int) arbitrary)
                  [ proper skew
                  , readShow
                  , equalityProps
                  , ordProps
                  , monoidProps
                  , functorLaws (Proxy :: Proxy Int) (Proxy :: Proxy Int)]
            , testGroup
                  "Indexed Braun"
                  [indexedSort (Proxy :: Proxy Indexed.Braun)]
            , testGroup
                  "Indexed Binomial"
                  [indexedSort (Proxy :: Proxy (Indexed.Binomial 0))]
            , testGroup
                  "Indexed Leftist"
                  [indexedSort (Proxy :: Proxy Indexed.Leftist)]
            , testGroup
                  "Indexed Pairing"
                  [indexedSort (Proxy :: Proxy Indexed.Pairing)]
            , testGroup
                  "Indexed Skew"
                  [indexedSort (Proxy :: Proxy Indexed.Skew)]
            , testGroup "Binary Tree" $
              withGen
                  intTree
                  [ readShow
                  , readShow1
                  , equalityProps
                  , liftedEq
                  , ordProps
                  , liftedOrd
                  , monoidProps
                  , functorLaws (Proxy :: Proxy Int) (Proxy :: Proxy Int)]]

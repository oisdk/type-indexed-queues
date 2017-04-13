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
import qualified Data.Tree as Rose
import qualified Data.Tree.Replicate as Rose


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
import           Data.Monoid

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

propHeapSort :: (PriorityQueue h Int) => Proxy h -> TestTree
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

main :: IO ()
main = do
    doctest ["-isrc", "src"]
    defaultMain $
        testGroup
            "Tests"
            [ let xs = fmap (fromList :: [Int] -> Binomial 'Z Int) arbitrary
              in testGroup
                     "Binomial"
                     [ testProperty "proper" (properBinomial . fromList')
                     , propHeapSort (Proxy :: Proxy (Binomial 'Z))
                     , readShow xs
                     , equalityProps xs
                     , ordProps xs
                     , monoidProps xs
                     , functorLaws (Proxy :: Proxy Int) (Proxy :: Proxy Int) xs]
            ,let xs = fmap (fromList :: [Int] -> Braun Int) arbitrary
             in testGroup
                  "Braun"
                  [ testProperty
                        "proper"
                        (properBraun . fromList :: [Int] -> Bool)
                  , propHeapSort (Proxy :: Proxy Braun)
                  , readShow xs
                  , equalityProps xs
                  , ordProps xs
                  , functorLaws (Proxy :: Proxy Int) (Proxy :: Proxy Int) xs]
            , testGroup "Leftist" $
              propHeapSort (Proxy :: Proxy Leftist) :
              withGen
                  (fmap (fromList' :: [Int] -> Leftist Int) arbitrary)
                  [ readShow
                  , equalityProps
                  , ordProps
                  , monoidProps
                  , functorLaws (Proxy :: Proxy Int) (Proxy :: Proxy Int)]
            , testGroup "Pairing" [propHeapSort (Proxy :: Proxy Pairing)]
            , testGroup "Skew" [propHeapSort (Proxy :: Proxy Skew)]
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
                  , functorLaws (Proxy :: Proxy Int) (Proxy :: Proxy Int)]
            ,
              -- , applicativeLaws (Proxy :: Proxy Int) (Proxy :: Proxy Int)
              -- , monadLaws
              --       (fmap
              --            (treeFromList .)
              --            (arbitrary :: Gen (Int -> [Int])))
              --       (fmap
              --            (treeFromList .)
              --            (arbitrary :: Gen (Int -> [Int])))]
              testGroup
                  "Rose Tree" $
              withGen
                  intRoseTree
                  [ readShow
                  , equalityProps]]

-- seqRightLaw
--     :: (Applicative f, Eq (f b), Show (f b))
--     => f a -> f b -> Property
-- seqRightLaw xs ys = (xs *> ys) === liftA2 (const id) xs ys

-- seqLeftLaw
--     :: (Applicative f, Eq (f a), Show (f a))
--     => f a -> f b -> Property
-- seqLeftLaw xs ys = (xs <* ys) === liftA2 const xs ys

-- appIdLaw
--     :: (Applicative f, Eq (f a), Show (f a))
--     => f a -> Property
-- appIdLaw xs = (pure id <*> xs) === xs

-- appCompLaw
--     :: (Applicative f
--        ,Eq (f c)
--        ,Show (f c))
--     => Blind (f (b -> c)) -> Blind (f (a -> b)) -> f a -> Property
-- appCompLaw (Blind u) (Blind v) w
--   = (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

-- homomorphismLaw
--     :: (Applicative f
--        ,Eq (f b)
--        ,Show (f b))
--     => Proxy f -> Blind (a -> b) -> a -> Property
-- homomorphismLaw (_ :: Proxy f) (Blind (f :: a -> b)) x
--   = (pure f <*> pure x) === (pure (f x) :: f b)

-- interchangeLaw
--     :: (Applicative f
--        ,Eq (f b)
--        ,Show (f b))
--     => Blind (f (a -> b)) -> a -> Property
-- interchangeLaw (Blind u) y = (u <*> pure y) === (pure ($y) <*> u)

-- applicativeLaws
--     :: (Applicative f
--        ,Arbitrary a
--        ,Show a
--        ,Eq (f b)
--        ,Show (f b)
--        ,Eq (f a)
--        ,Show (f a)
--        ,Traversable f
--        ,CoArbitrary a
--        ,CoArbitrary b
--        ,Arbitrary c
--        ,Arbitrary b
--        ,Eq (f c)
--        ,Show (f c))
--     => p b -> q c -> Gen (f a) -> TestTree
-- applicativeLaws (_ :: p b) (_ :: q c) (xs :: Gen (f a)) =
--     testGroup
--         "applicative laws"
--         [ testProperty "*>" $ seqRightLaw <$> xs <*> xs
--         , testProperty "<*" $ seqLeftLaw <$> xs <*> xs
--         , testProperty "app identity" $ appIdLaw <$> xs
--         , testProperty "app comp" $
--           do bc <- traverse (const (arbitrary :: Gen (b -> c))) =<< xs
--              ab <- traverse (const arbitrary) =<< xs
--              fa <- xs
--              pure $ appCompLaw (Blind bc) (Blind ab) fa
--         , testProperty "homomorphism" $
--           homomorphismLaw (Proxy :: Proxy f) <$>
--           (arbitrary :: Gen (Blind (a -> b)))
--         , testProperty "interchange" $
--           interchangeLaw <$>
--           (Blind <$> (traverse (const (arbitrary :: Gen (a -> b))) =<< xs)) <*>
--           (arbitrary :: Gen a)]



-- monadLawOne
--     :: (Monad f, Show (f b), Eq (f b))
--     => Blind (a -> f b) -> a -> Property
-- monadLawOne (Blind k) a = (pure a >>= k) === k a

-- monadLawTwo
--     :: (Monad f, Eq (f a), Show (f a))
--     => f a -> Property
-- monadLawTwo xs = (xs >>= pure) === xs

-- monadLawThree
--     :: (Monad f, Eq (f c), Show (f c))
--     => f a -> Blind (a -> f b) -> Blind (b -> f c) -> Property
-- monadLawThree m (Blind k) (Blind h) = (m >>= (k >=> h)) === ((m >>= k) >>= h)

-- monadLaws
--     :: (Monad f
--        ,Arbitrary a
--        ,Show a
--        ,Eq (f b)
--        ,Show (f b)
--        ,Eq (f a)
--        ,Show (f a)
--        ,Traversable f
--        ,CoArbitrary a
--        ,CoArbitrary b
--        ,Arbitrary c
--        ,Arbitrary b
--        ,Eq (f c)
--        ,Show (f c))
--     => Gen (a -> f b) -> Gen (b -> f c) -> Gen (f a) -> TestTree
-- monadLaws (fs :: Gen (a -> f b)) (gs :: Gen (b -> f c)) (xs :: Gen (f a)) =
--     testGroup
--         "monad laws"
--         [ testProperty "return a >>= k  =  k a" $
--           monadLawOne <$> fmap Blind fs <*> arbitrary
--         , testProperty "m >>= return  =  m" $ monadLawTwo <$> xs
--         , testProperty "m >>= (\\x -> k x >>= h)  =  (m >>= k) >>= h" $
--           monadLawThree <$> xs <*> fmap Blind fs <*> fmap Blind gs
--         , testProperty "ap = <*>" $
--           do x <- xs
--              y <- xs
--              f <- arbitrary :: Gen (a -> a -> c)
--              pure $ ((f <$> x) <*> y) === ((f <$> x) `ap` y)]

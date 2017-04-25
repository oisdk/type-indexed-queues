{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | Type-level Peano arithmetic.
module TypeLevel.Nat where

import TypeLevel.Singletons hiding (type (+),Nat)
import Data.List (unfoldr)

import Control.DeepSeq (NFData(rnf))

import GHC.Generics (Generic)
import Data.Data (Data,Typeable)

-- $setup
-- >>> import Test.QuickCheck
-- >>> :{
-- instance Arbitrary Nat where
--     arbitrary = fmap (fromInteger . getNonNegative) arbitrary
-- :}

-- | Peano numbers. Care is taken to make operations as lazy as
-- possible:
--
-- >>> 1 > S (S undefined)
-- False
-- >>> Z > undefined
-- False
-- >>> 3 + (undefined :: Nat) >= 3
-- True
data Nat
    = Z
    | S Nat
    deriving (Eq,Generic,Data,Typeable)

-- | As lazy as possible
instance Ord Nat where
    compare Z Z = EQ
    compare (S n) (S m) = compare n m
    compare Z (S _) = LT
    compare (S _) Z = GT
    min Z _ = Z
    min (S n) (S m) = S (min n m)
    min _ Z = Z
    max Z m = m
    max (S n) (S m) = S (max n m)
    max n Z = n
    Z <= _ = True
    S n <= S m = n <= m
    S _ <= Z = False
    Z > _ = False
    S n > S m = n > m
    S _ > Z = True
    _ >= Z = True
    Z >= S _ = False
    S n >= S m = n >= m
    _ < Z = False
    S n < S m = n < m
    Z < S _ = True

-- | Singleton for type-level Peano numbers.
data instance The Nat n where
    Zy :: The Nat Z
    Sy :: The Nat n -> The Nat (S n)

-- | Add two type-level numbers.
infixl 6 +
type family (+) (n :: Nat) (m :: Nat) :: Nat where
    Z + m = m
    S n + m = S (n + m)

-- | Subtraction stops at zero.
--
-- prop> n >= m ==> m - n == Z
instance Num Nat where
    Z + m = m
    S n + m = S (n + m)
    Z * _ = Z
    S n * m = m + n * m
    abs = id
    signum Z = 0
    signum _ = 1
    fromInteger = go . abs
      where
        go 0 = Z
        go n = S (go (n-1))
    S n - S m = n - m
    n - _ = n

-- | The maximum bound here is infinity.
--
-- prop> (maxBound :: Nat) > n
instance Bounded Nat where
    minBound = Z
    maxBound = S maxBound

-- | Uses custom 'enumFrom', 'enumFromThen', 'enumFromThenTo' to avoid
-- expensive conversions to and from 'Int'.
--
-- >>> [1..3] :: [Nat]
-- [1,2,3]
-- >>> [1..1] :: [Nat]
-- [1]
-- >>> [2..1] :: [Nat]
-- []
-- >>> take 3 [1,2..] :: [Nat]
-- [1,2,3]
-- >>> take 3 [5,4..] :: [Nat]
-- [5,4,3]
-- >>> [1,3..7] :: [Nat]
-- [1,3,5,7]
-- >>> [5,4..1] :: [Nat]
-- [5,4,3,2,1]
-- >>> [5,3..1] :: [Nat]
-- [5,3,1]
instance Enum Nat where
    succ = S
    pred (S n) = n
    pred Z = error "pred called on zero nat"
    fromEnum = go 0
      where
        go !n Z = n
        go !n (S m) = go (1 + n) m
    toEnum = go . abs
      where
        go 0 = Z
        go n = S (go (n-1))
    enumFrom = iterate S
    enumFromTo n m = unfoldr f (n, S m - n)
      where
        f (_,Z) = Nothing
        f (e,S l) = Just (e, (S e, l))
    enumFromThen n m = iterate t n
      where
        ts Z mm = (+) mm
        ts (S nn) (S mm) = ts nn mm
        ts nn Z = subtract nn
        t = ts n m
    enumFromThenTo n m t =
        unfoldr
            f
            (n,either (const (S n - t)) (const (S t - n)) tt)
      where
        ts Z mm = Right mm
        ts (S nn) (S mm) = ts nn mm
        ts nn Z = Left nn
        tt = ts n m
        tf = either subtract (+) tt
        td = either subtract subtract tt
        f (_,Z) = Nothing
        f (e,l@(S _)) = Just (e, (tf e,td l))

-- | Reasonably expensive.
instance Real Nat where
    toRational = fromInteger . toInteger

-- | Not at all optimized.
--
-- >>> 5 `div` 2 :: Nat
-- 2
instance Integral Nat where
    toInteger = go 0
      where
        go !p Z = p
        go !p (S n) = go (p + 1) n
    quotRem _ Z = error "divide by zero"
    quotRem x (S y) = qr Z x (S y)
      where
        qr q n m = go n m
          where
            go nn Z = qr (S q) nn m
            go (S nn) (S mm) = go nn mm
            go Z (S _) = (q, n)
    div _ Z = error "divide by zero"
    div n m = go n where
      go = subt m where
        subt Z nn = S (go nn)
        subt (S mm) (S nn) = subt mm nn
        subt (S _) Z = Z


-- | Shows integer representation.
instance Show Nat where
    showsPrec n = showsPrec n . toInteger

-- | Reads the integer representation.
instance Read Nat where
    readsPrec d r =
        [ (fromInteger n, xs)
        | (n,xs) <- readsPrec d r ]

-- | Will obviously diverge for values like `maxBound`.
instance NFData Nat where
    rnf Z = ()
    rnf (S n) = rnf n

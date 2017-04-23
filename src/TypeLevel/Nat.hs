{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns  #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | Type-level Peano arithmetic.
module TypeLevel.Nat where

import TypeLevel.Singletons hiding (type (+),Nat)

-- | Peano numbers.
data Nat = Z | S Nat deriving (Eq, Ord)

-- | Singleton for type-level Peano numbers.
data instance The Nat n where
    Zy :: The Nat Z
    Sy :: The Nat n -> The Nat (S n)

-- | Add two type-level numbers.
infixl 6 +
type family (+) (n :: Nat) (m :: Nat) :: Nat where
    Z + m = m
    S n + m = S (n + m)

instance Num Nat where
    Z + m = m
    S n + m = S (n + m)
    Z * _ = Z
    S n * m = m + n * m
    abs = id
    signum Z = 0
    signum _ = 1
    fromInteger = go . abs where
      go 0 = Z
      go n = case quotRem n 2 of
        (m,0) -> S (S Z) * go m
        (m,_) -> S (S (S Z) * go m)
    S n - S m = n - m
    n - _ = n

instance Enum Nat where
    succ = S
    pred (S n) = n
    pred Z = error "pred called on zero nat"
    fromEnum = go 0 where
      go !n Z = n
      go !n (S m) = go (1 + n) m
    toEnum = go . abs where
      go 0 = Z
      go n = case quotRem n 2 of
        (m,0) -> S (S Z) * go m
        (m,_) -> S (S (S Z) * go m)

instance Show Nat where
    showsPrec n m = showsPrec n (go 0 m) where
      go :: Integer -> Nat -> Integer
      go !p Z = p
      go !p (S v) = go (1 + p) v

{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

-- | Type-level Peano arithmetic.
module TypeLevel.Nat where

import TypeLevel.Singletons hiding (type (+),Nat)

-- | Peano numbers.
data Nat = Z | S Nat

-- | Singleton for type-level Peano numbers.
data instance The Nat n where
    Zy :: The Nat 'Z
    Sy :: The Nat n -> The Nat ('S n)

-- | Add two type-level numbers.
infixl 6 +
type family (+) (n :: Nat) (m :: Nat) :: Nat where
    'Z + m = m
    'S n + m = 'S (n + m)

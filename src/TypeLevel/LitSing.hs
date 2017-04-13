{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeLevel.LitSing
  ((+)
  ,(*)
  ,(^)
  ,(<=.)
  ,totalOrder)
  where

import GHC.TypeLits
import Prelude hiding (Num(..), (^))
import qualified Prelude
import Data.Coerce
import TypeLevel.Singletons
import Data.Type.Equality
import Unsafe.Coerce

import Data.Proxy
import Numeric.Natural

-- | A type-level number, backed by an integer.
newtype instance The Nat n where
        NatSing :: Natural -> The Nat n

instance KnownNat n => KnownSing n where
    sing = NatSing $ Prelude.fromInteger $ natVal (Proxy :: Proxy n)

infixl 6 +
(+) :: The Nat n -> The Nat m -> The Nat (n + m)
(+) =
    (coerce :: (Natural -> Natural -> Natural) -> The Nat n -> The Nat m -> The Nat (n + m))
        (Prelude.+)
{-# INLINE (+) #-}

infixl 7 *
(*) :: The Nat n -> The Nat m -> The Nat (n * m)
(*) =
    (coerce :: (Natural -> Natural -> Natural) -> The Nat n -> The Nat m -> The Nat (n * m))
        (Prelude.*)
{-# INLINE (*) #-}

infixr 8 ^
(^) :: The Nat n -> The Nat m -> The Nat (n ^ m)
(^) =
    (coerce :: (Natural -> Natural -> Natural) -> The Nat n -> The Nat m -> The Nat (n ^ m))
        (Prelude.^)
{-# INLINE (^) #-}

infix 4 <=.
(<=.) :: The Nat n -> The Nat m -> The Bool (n <=? m)
(<=.) (NatSing x :: The Nat n) (NatSing y :: The Nat m)
  | x <= y = case (unsafeCoerce (Refl :: 'True :~: 'True) :: (n <=? m) :~: 'True) of
      Refl -> Truey
  | otherwise = case (unsafeCoerce (Refl :: 'True :~: 'True) :: (n <=? m) :~: 'False) of
      Refl -> Falsy
{-# INLINE (<=.) #-}

totalOrder :: Proxy n -> Proxy m -> ((n <=? m) :~: 'False) -> ((m <=? n) :~: 'True)
totalOrder _ _ Refl = unsafeCoerce Refl

{-# LANGUAGE TypeInType    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeLevel.Singletons
  (The(Truey, Falsy, Nily, (:-))
  ,KnownSing(..)
  ,(*.)
  ,(+.)
  ,(^.)
  ,(<=.)
  ,totalOrder)
  where

import Data.Kind
import Numeric.Natural
import qualified GHC.TypeLits as Lit
import Data.Proxy
import Data.Type.Equality

import Data.Coerce
import Unsafe.Coerce

data family The k :: k -> *

data instance The Bool x where
    Falsy :: The Bool 'False
    Truey :: The Bool 'True

infixr 5 :-
data instance The [k] xs where
    Nily :: The [k] '[]
    (:-) :: The k x -> The [k] xs -> The [k] (x ': xs)

class KnownSing (x :: k) where
    sing :: The k x

instance KnownSing 'True where
    sing = Truey

instance KnownSing 'False where
    sing = Falsy

instance KnownSing '[] where
    sing = Nily

instance (KnownSing xs, KnownSing x) =>
         KnownSing (x ': xs) where
    sing = sing :- sing

newtype instance The Lit.Nat n where
        NatSing :: Natural -> The Lit.Nat n

instance Lit.KnownNat n => KnownSing n where
    sing = NatSing $ Prelude.fromInteger $ Lit.natVal (Proxy :: Proxy n)

infixl 6 +.
(+.) :: The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.+ m)
(+.) =
    (coerce :: (Natural -> Natural -> Natural) -> The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.+ m))
        (Prelude.+)
{-# INLINE (+.) #-}

infixl 7 *.
(*.) :: The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.* m)
(*.) =
    (coerce :: (Natural -> Natural -> Natural) -> The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.* m))
        (Prelude.*)
{-# INLINE (*.) #-}

infixr 8 ^.
(^.) :: The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.^ m)
(^.) =
    (coerce :: (Natural -> Natural -> Natural) -> The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.^ m))
        (Prelude.^)
{-# INLINE (^.) #-}

infix 4 <=.
(<=.) :: The Lit.Nat n -> The Lit.Nat m -> The Bool (n Lit.<=? m)
(<=.) (NatSing x :: The Lit.Nat n) (NatSing y :: The Lit.Nat m)
  | x <= y = case (unsafeCoerce (Refl :: 'True :~: 'True) :: (n Lit.<=? m) :~: 'True) of
      Refl -> Truey
  | otherwise = case (unsafeCoerce (Refl :: 'True :~: 'True) :: (n Lit.<=? m) :~: 'False) of
      Refl -> Falsy
{-# INLINE (<=.) #-}

totalOrder :: Proxy n -> Proxy m -> ((n Lit.<=? m) :~: 'False) -> ((m Lit.<=? n) :~: 'True)
totalOrder _ _ Refl = unsafeCoerce Refl

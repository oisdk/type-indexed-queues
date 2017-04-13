{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}

-- | Provides singletons and general type-level utilities. singletons
-- are value-level representations of types.
--
-- <http://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf Eisenberg, Richard A., and Stephanie Weirich. “Dependently Typed Programming with Singletons.” In Proceedings of the 2012 Haskell Symposium, 117–130. Haskell ’12. New York, NY, USA: ACM, 2012. doi:10.1145/2364506.2364522.>
module TypeLevel.Singletons
  (The(Truey, Falsy, Nily, (:-))
  ,KnownSing(..)
  ,(*.)
  ,(+.)
  ,(^.)
  ,(<=.)
  ,totalOrder
  ,type (<=)
  ,type (Lit.+)
  ,type (Lit.*)
  ,type (Lit.^)
  ,Lit.Nat)
  where

import Data.Kind
import Numeric.Natural
import qualified GHC.TypeLits as Lit
import Data.Proxy
import Data.Type.Equality

import Data.Coerce
import Unsafe.Coerce

-- | A data family for singletons. The cute name allows code like this:
--
-- @addZeroZero :: The Nat n -> n + 0 :~: n@
--
data family The k :: k -> *

data instance The Bool x where
    Falsy :: The Bool 'False
    Truey :: The Bool 'True

infixr 5 :-
data instance The [k] xs where
    Nily :: The [k] '[]
    (:-) :: The k x -> The [k] xs -> The [k] (x ': xs)

-- | Class for singletons which can be generated.
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

-- | This is just a newtype wrapper for 'Numeric.Natural.Natural'. As such, it
-- is only valid if the programmer can't construct values where the
-- type index doesn't match the contained value. For that reason,
-- the constructor is not exported.
--
-- The reason for this setup is to allow properties and invariants to be proven
-- about the numbers involved, while actual computation can  be carried out
-- efficiently on the values at runtime.
--
-- See the implementation of "Data.Heap.Indexed.Leftist" for an example of
-- the uses of this type.
newtype instance The Lit.Nat n where
        NatSing :: Natural -> The Lit.Nat n

instance Lit.KnownNat n => KnownSing n where
    sing = NatSing $ Prelude.fromInteger $ Lit.natVal (Proxy :: Proxy n)

-- | Add two numbers, on both the value and type level.
infixl 6 +.
(+.) :: The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.+ m)
(+.) =
    (coerce :: (Natural -> Natural -> Natural) -> The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.+ m))
        (Prelude.+)
{-# INLINE (+.) #-}

-- | Multiply two numbers, on both the value and type level.
infixl 7 *.
(*.) :: The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.* m)
(*.) =
    (coerce :: (Natural -> Natural -> Natural) -> The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.* m))
        (Prelude.*)
{-# INLINE (*.) #-}

-- | Raise a number to a power, on the type-level and value-level.
infixr 8 ^.
(^.) :: The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.^ m)
(^.) =
    (coerce :: (Natural -> Natural -> Natural) -> The Lit.Nat n -> The Lit.Nat m -> The Lit.Nat (n Lit.^ m))
        (Prelude.^)
{-# INLINE (^.) #-}

-- | Test order between two numbers, and provide a proof of that
-- order with the result.
infix 4 <=.
(<=.) :: The Lit.Nat n -> The Lit.Nat m -> The Bool (n Lit.<=? m)
(<=.) (NatSing x :: The Lit.Nat n) (NatSing y :: The Lit.Nat m)
  | x <= y = case (unsafeCoerce (Refl :: 'True :~: 'True) :: (n Lit.<=? m) :~: 'True) of
      Refl -> Truey
  | otherwise = case (unsafeCoerce (Refl :: 'True :~: 'True) :: (n Lit.<=? m) :~: 'False) of
      Refl -> Falsy
{-# INLINE (<=.) #-}

-- | A proof of a total order on the naturals.
totalOrder ::  p n -> q m -> (n Lit.<=? m) :~: 'False -> (m Lit.<=? n) :~: 'True
totalOrder (_ :: p n) (_ :: q m) Refl = unsafeCoerce Refl :: (m Lit.<=? n) :~: 'True

-- | A proof that x is less than or equal to y.
type x <= y = (x Lit.<=? y) :~: 'True

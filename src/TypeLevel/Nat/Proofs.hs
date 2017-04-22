{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | Some proofs on type-level Peano numbers.
module TypeLevel.Nat.Proofs where

import TypeLevel.Nat
import Data.Type.Equality
import Unsafe.Coerce

-- | Addition is associative.
plusAssoc :: The Nat x -> p y -> q z -> ((x + y) + z) :~: (x + (y + z))
plusAssoc Zy _ _ = Refl
plusAssoc (Sy x) y z = case plusAssoc x y z of
  Refl -> Refl
{-# NOINLINE plusAssoc #-}

-- | Zero is the identity of addition.
plusZeroNeutral :: The Nat n -> n + Z :~: n
plusZeroNeutral Zy = Refl
plusZeroNeutral (Sy n) = case plusZeroNeutral n of
  Refl -> Refl
{-# NOINLINE plusZeroNeutral #-}

-- | Successor distributes over addition
plusSuccDistrib :: The Nat n -> proxy m -> n + S m :~: S (n + m)
plusSuccDistrib Zy _ = Refl
plusSuccDistrib (Sy n) p = gcastWith (plusSuccDistrib n p) Refl
{-# NOINLINE plusSuccDistrib #-}


{-# RULES
"plusAssoc" forall x y z. plusAssoc x y z = unsafeCoerce (Refl :: 'Z :~: 'Z)
"plusZeroNeutral" forall x. plusZeroNeutral x = unsafeCoerce (Refl :: 'Z :~: 'Z)
"plusSuccDistrib" forall x y. plusSuccDistrib x y = unsafeCoerce (Refl :: 'Z :~: 'Z)
 #-}

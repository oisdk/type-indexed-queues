{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DataKinds     #-}

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
addZeroZero :: The Nat x -> x + 'Z :~: x
addZeroZero Zy = Refl
addZeroZero (Sy x) = case addZeroZero x of
  Refl -> Refl
{-# NOINLINE addZeroZero #-}

{-# RULES
"plusAssoc" forall x y z. plusAssoc x y z = unsafeCoerce (Refl :: 'Z :~: 'Z)
"addZeroZero" forall x. addZeroZero x = unsafeCoerce (Refl :: 'Z :~: 'Z)
 #-}

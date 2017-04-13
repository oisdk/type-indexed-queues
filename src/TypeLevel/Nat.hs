{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module TypeLevel.Nat where

import TypeLevel.Singletons

data Nat = Z | S Nat

data instance The Nat n where
    Zy :: The Nat 'Z
    Sy :: The Nat n -> The Nat ('S n)

infixl 6 +
type family (+) (n :: Nat) (m :: Nat) :: Nat where
    'Z + m = m
    'S n + m = 'S (n + m)

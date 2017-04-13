{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RebindableSyntax #-}

-- | Rebinable syntax helper.
module TypeLevel.Bool where

import           TypeLevel.Singletons
import           Data.Type.Equality

import           Prelude

-- | For use with '-XRebindableSyntax'.
ifThenElse :: The Bool c -> (c :~: 'True -> a) -> (c :~: 'False -> a) -> a
ifThenElse Truey t _ = t Refl
ifThenElse Falsy _ f = f Refl

type family IfThenElse (c :: Bool) (true :: k) (false :: k) :: k
     where
        IfThenElse 'True true false = true
        IfThenElse 'False true false = false

depHask :: The Bool x -> IfThenElse x Int String
depHask cond =
    if cond
        then \Refl ->
                  1
        else \Refl ->
                  "abc"

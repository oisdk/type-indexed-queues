{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

-- | Rebinable syntax helper.
module TypeLevel.Bool where

import           Data.Type.Equality
import           TypeLevel.Singletons

import           Prelude

-- | For use with @-XRebindableSyntax@. This function can be used to
-- make Haskell look reasonably dependent:
--
-- @
-- depHask :: 'The' 'Bool' x -> 'IfThenElse' x 'Int' 'String'
-- depHask cond =
--     if cond
--         then \\'Refl' -> 1
--         else \\'Refl' -> "abc"
-- @
ifThenElse :: The Bool c -> (c :~: 'True -> a) -> (c :~: 'False -> a) -> a
ifThenElse Truey t _ = t Refl
ifThenElse Falsy _ f = f Refl

-- | Type-level if then else.
type family IfThenElse (c :: Bool) (true :: k) (false :: k) :: k
     where
        IfThenElse 'True true false = true
        IfThenElse 'False true false = false

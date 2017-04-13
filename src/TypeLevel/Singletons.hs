{-# LANGUAGE TypeInType    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe          #-}

module TypeLevel.Singletons where

import Data.Kind

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

instance (KnownSing xs, KnownSing x) => KnownSing (x ': xs) where
    sing = sing :- sing

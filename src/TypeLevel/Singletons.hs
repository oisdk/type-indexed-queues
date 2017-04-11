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

class KnownSing x where
    sing :: The k x

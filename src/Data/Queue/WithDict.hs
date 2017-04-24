{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveDataTypeable    #-}

-- | Provides a wrapper for queues, allowing them to conform to 'Foldable'.
module Data.Queue.WithDict
  (WithDict(..))
  where

import           Data.Queue.Class
import           Data.Proxy

import           Control.DeepSeq (NFData(rnf))
import           Data.Data       (Data)
import           Data.Typeable   (Typeable)

-- | This stores the dictionary of methods for the
-- priority queue of @f@, allowing the entire type
-- to conform to 'Foldable'.
data WithDict f a where
    WithDict :: Queue f a => f a -> WithDict f a

instance Queue f a => Queue (WithDict f) a where
    minView (WithDict xs) = (fmap.fmap) WithDict (minView xs)
    insert x (WithDict xs) = WithDict (insert x xs)
    empty = WithDict empty
    singleton = WithDict . singleton
    toList (WithDict xs) = toList xs
    fromList = WithDict . fromList
    heapSort (_ :: p (WithDict h)) = heapSort (Proxy :: Proxy h)

instance MeldableQueue f a => MeldableQueue (WithDict f) a where
    merge (WithDict xs) (WithDict ys) = WithDict (merge xs ys)
    fromFoldable = WithDict . fromFoldable

instance Foldable (WithDict f) where
    foldr f b (WithDict xs) = go xs where
      go hs = case minView hs of
        Nothing     -> b
        Just (y,ys) -> f y (go ys)
    foldMap f (WithDict xs) = go xs where
      go hs = case minView hs of
        Nothing     -> mempty
        Just (y,ys) -> f y `mappend` go ys

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance NFData (f a) => NFData (WithDict f a) where
    rnf (WithDict x) = rnf x `seq` ()

deriving instance
         (Data a, Data (f a), Typeable f, Queue f a) => Data
         (WithDict f a)
deriving instance Typeable (WithDict f a)


instance (Eq a, Queue f a) => Eq (WithDict f a) where
    (==) = eqQueue

instance (Ord a, Queue f a) => Ord (WithDict f a) where
    compare = cmpQueue

instance (Show a, Queue f a) => Show (WithDict f a) where
    showsPrec = showsPrecQueue

instance (Read a, Queue f a) => Read (WithDict f a) where
    readsPrec = readPrecQueue

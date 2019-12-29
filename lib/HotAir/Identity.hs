{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Identity
  ( Identity,
    identity,
    runIdentity
    )
where

import Control.Applicative (Applicative ((<*>), pure))
import Control.Monad (Monad ((>>=)))
import Data.Function (id)
import Data.Functor ((<$>), Functor)

newtype Identity a
  = Identity (forall r. (a -> r) -> r)
  deriving (Functor)

instance Applicative Identity where

  pure :: a -> Identity a
  pure = identity

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  a <*> b = runIdentity a <$> b

instance Monad Identity where
  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  a >>= f = f (runIdentity a)

identity :: a -> Identity a
identity a = Identity (\a2r -> a2r a)

runIdentity :: Identity a -> a
runIdentity (Identity i) = i id

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module HotAir.Identity
  ( Identity
  , identity
  , runIdentity
  ) where

newtype Identity a =
  Identity (forall c. (a -> c) -> c)

identity :: a -> Identity a
identity a = Identity (\i -> i a)

runIdentity :: Identity a -> a
runIdentity (Identity i) = i (\a -> a)

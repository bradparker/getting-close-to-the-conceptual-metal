{-# LANGUAGE NoImplicitPrelude #-}

module HotAir.Semigroup
  ( Semigroup((<>))
  ) where

class Semigroup a where
  (<>) :: a -> a -> a

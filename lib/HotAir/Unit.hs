{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module HotAir.Unit
  ( Unit
  , unit
  ) where

newtype Unit =
  Unit (forall c. c -> c)

unit :: Unit
unit = Unit (\a -> a)

{-# LANGUAGE NoImplicitPrelude #-}

module HotAir.Char
  ( Char
  , fromBuiltin
  , toBuiltin
  ) where

import qualified Data.Char as Builtin

import HotAir.Function ((.))
import HotAir.Nat (Nat, fromNum, toNum)

newtype Char =
  Char Nat

fromBuiltin :: Builtin.Char -> Char
fromBuiltin = Char . fromNum . Builtin.ord

toBuiltin :: Char -> Builtin.Char
toBuiltin (Char c) = Builtin.chr (toNum c)

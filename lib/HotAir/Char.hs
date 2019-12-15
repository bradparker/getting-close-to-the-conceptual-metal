{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Char
  ( Char,
    fromBuiltin,
    toBuiltin
    )
where

import qualified Data.Char as Builtin
import Data.Function ((.))
import HotAir.Eq (Eq)
import HotAir.Nat (Nat, fromNum, toNum)

newtype Char
  = Char Nat
  deriving (Eq)

fromBuiltin :: Builtin.Char -> Char
fromBuiltin = Char . fromNum . Builtin.ord

toBuiltin :: Char -> Builtin.Char
toBuiltin (Char c) = Builtin.chr (toNum c)

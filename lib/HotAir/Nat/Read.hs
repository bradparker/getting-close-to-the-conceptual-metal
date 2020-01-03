{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Nat.Read
  ( readNat
    )
where

import Data.Function ((.))
import Data.Functor (fmap)
import Data.Traversable (traverse)
import HotAir.Char (digitToNat)
import HotAir.List (foldr, reverse)
import HotAir.Nat (Nat)
import GHC.Num ((+), (*))
import HotAir.Maybe (Maybe)
import HotAir.String (String)
import qualified HotAir.String as String

readNat :: String -> Maybe Nat
readNat =
  fmap (foldr (\n acc -> acc * 10 + n) 0 . reverse)
    . traverse digitToNat
    . String.toList

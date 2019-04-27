{-# LANGUAGE NoImplicitPrelude #-}

module HotAir.Eq
  ( Eq((==))
  ) where

import qualified Data.Eq as Builtin
import GHC.Int (Int)
import GHC.Integer (Integer)

import HotAir.Bool (Bool, fromBuiltin)

class Eq a where
  (==) :: a -> a -> Bool

instance Eq Int where
  a == b = fromBuiltin (a Builtin.== b)

instance Eq Integer where
  a == b = fromBuiltin (a Builtin.== b)

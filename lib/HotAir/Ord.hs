{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Ord
  ( Ord ((<=))
    )
where

import qualified Data.Ord as Builtin
import GHC.Int (Int)
import GHC.Integer (Integer)
import HotAir.Bool (Bool, fromBuiltin)
import HotAir.Eq (Eq)

class Eq a => Ord a where

  (<=) :: a -> a -> Bool

instance Ord Int where
  a <= b = fromBuiltin (a Builtin.<= b)

instance Ord Integer where
  a <= b = fromBuiltin (a Builtin.<= b)

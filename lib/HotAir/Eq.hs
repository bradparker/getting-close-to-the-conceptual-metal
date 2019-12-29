{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Eq
  ( Eq ((==), (/=))
    )
where

import qualified Data.Eq as Builtin
import GHC.Int (Int)
import GHC.Integer (Integer)
import GHC.Natural (Natural)
import HotAir.Bool ((&&), Bool, fromBuiltin, not, (||))

class Eq a where

  (==) :: a -> a -> Bool

  (/=) :: a -> a -> Bool
  a /= b = not (a == b)

instance Eq Int where
  a == b = fromBuiltin (a Builtin.== b)

instance Eq Integer where
  a == b = fromBuiltin (a Builtin.== b)

instance Eq Natural where
  a == b = fromBuiltin (a Builtin.== b)

instance Eq Bool where
  a == b = (a && b) || not (a || b)

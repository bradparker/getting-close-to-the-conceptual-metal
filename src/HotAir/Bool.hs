{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module HotAir.Bool
  ( Bool
  , true
  , false
  , ifThenElse
  , fromBuiltin
  ) where

import qualified Data.Bool as Builtin

newtype Bool =
  Bool (forall c. c -> c -> c)

true :: Bool
true = Bool (\t _ -> t)

false :: Bool
false = Bool (\_ f -> f)

ifThenElse :: Bool -> a -> a -> a
ifThenElse (Bool b) = b

toBuiltin :: Bool -> Builtin.Bool
toBuiltin b = ifThenElse b Builtin.True Builtin.False

fromBuiltin :: Builtin.Bool -> Bool
fromBuiltin = Builtin.bool false true

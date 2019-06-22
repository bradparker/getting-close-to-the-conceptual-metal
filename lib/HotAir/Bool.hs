{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module HotAir.Bool
  ( Bool
  , true
  , false
  , (||)
  , (&&)
  , not
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

not :: Bool -> Bool
not (Bool b) = Bool (\t f -> b f t)

(||) :: Bool -> Bool -> Bool
Bool a || Bool b = a (Bool a) (Bool b)

(&&) :: Bool -> Bool -> Bool
Bool a && Bool b = a (Bool b) (Bool a)

ifThenElse :: Bool -> a -> a -> a
ifThenElse (Bool b) = b

fromBuiltin :: Builtin.Bool -> Bool
fromBuiltin = Builtin.bool false true

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HotAir.String
  ( String
  , toBuiltin
  ) where

import qualified Data.List as Builtin
import qualified Data.String as Builtin

import HotAir.Char (Char)
import qualified HotAir.Char as Char
import HotAir.Function ((.))
import HotAir.List (List, cons, foldr, nil)
import HotAir.Semigroup (Semigroup)

newtype String =
  String (List Char)
  deriving (Semigroup)

toBuiltin :: String -> Builtin.String
toBuiltin (String chars) = foldr ((:) . Char.toBuiltin) [] chars

fromBuiltin :: Builtin.String -> String
fromBuiltin = String . Builtin.foldr (cons . Char.fromBuiltin) nil

instance Builtin.IsString String where
  fromString = fromBuiltin

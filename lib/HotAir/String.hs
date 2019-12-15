{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.String
  ( String,
    toBuiltin,
    toList,
    fromList,
    singleton
    )
where

import Data.Function ((.))
import qualified Data.List as Builtin
import Data.Semigroup (Semigroup)
import qualified Data.String as Builtin
import HotAir.Char (Char)
import qualified HotAir.Char as Char
import HotAir.List (List, cons, foldr, nil)
import qualified HotAir.List as List
import qualified Text.Show as Builtin

newtype String
  = String (List Char)
  deriving (Semigroup)

toList :: String -> List Char
toList (String s) = s

fromList :: List Char -> String
fromList = String

singleton :: Char -> String
singleton = fromList . List.singleton

toBuiltin :: String -> Builtin.String
toBuiltin (String chars) = foldr ((:) . Char.toBuiltin) [] chars

fromBuiltin :: Builtin.String -> String
fromBuiltin = String . Builtin.foldr (cons . Char.fromBuiltin) nil

instance Builtin.IsString String where
  fromString = fromBuiltin

instance Builtin.Show String where
  show = Builtin.show . toBuiltin

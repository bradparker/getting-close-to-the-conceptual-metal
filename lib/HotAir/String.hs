{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.String
  ( String,
    toList,
    fromList,
    singleton,
    length
    )
where

import qualified Data.Foldable as Foldable
import Data.Function ((.), const)
import Data.Semigroup (Semigroup)
import qualified Data.String as Builtin
import GHC.Num (Num ((+)))
import HotAir.Char (Char)
import qualified HotAir.Char as Char
import HotAir.List (List, cons, nil)
import qualified HotAir.List as List
import HotAir.Nat (Nat)

newtype String
  = String {toList :: List Char}
  deriving (Semigroup)

fromList :: List Char -> String
fromList = String

length :: String -> Nat
length = List.foldr (const (+ 1)) 0 . toList

singleton :: Char -> String
singleton = fromList . List.singleton

fromBuiltin :: Builtin.String -> String
fromBuiltin = String . Foldable.foldr (cons . Char.fromBuiltin) nil

instance Builtin.IsString String where
  fromString = fromBuiltin

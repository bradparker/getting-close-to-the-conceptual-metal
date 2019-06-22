{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HotAir.Show
  ( Show(show)
  ) where

import HotAir.Semigroup ((<>))
import HotAir.String (String)
import HotAir.Bool (Bool, ifThenElse)
import HotAir.Pair (Pair, fst, snd)

class Show a where
  show :: a -> String

instance Show String where
  show a = "\"" <> a <> "\""

instance Show Bool where
  show b = ifThenElse b "true" "false"

instance (Show a, Show b) => Show (Pair a b) where
  show p = "(pair " <> show (fst p) <> " " <> show (snd p) <> ")"

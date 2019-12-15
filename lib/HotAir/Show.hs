{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Show
  ( Show (show)
    )
where

import Data.Semigroup ((<>))
import HotAir.Bool (Bool, ifThenElse)
import HotAir.Char (Char)
import HotAir.Maybe (Maybe, maybe)
import HotAir.Pair (Pair, fst, snd)
import HotAir.String (String)
import qualified HotAir.String as String

class Show a where

  show :: a -> String

instance Show Char where
  show c = "'" <> String.singleton c <> "'"

instance Show String where
  show a = "\"" <> a <> "\""

instance Show Bool where
  show b = ifThenElse b "true" "false"

instance (Show a, Show b) => Show (Pair a b) where
  show p = "(pair " <> show (fst p) <> " " <> show (snd p) <> ")"

instance Show a => Show (Maybe a) where
  show = maybe "nothing" (\a -> "(just " <> show a <> ")")

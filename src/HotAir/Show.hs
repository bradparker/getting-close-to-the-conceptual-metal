{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HotAir.Show
  ( Show(show)
  ) where

import HotAir.Semigroup ((<>))
import HotAir.String (String)

class Show a where
  show :: a -> String

instance Show String where
  show a = "\"" <> a <> "\""

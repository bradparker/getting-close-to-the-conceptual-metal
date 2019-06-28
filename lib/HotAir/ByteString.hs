{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HotAir.ByteString
  ( ByteString
  ) where

import qualified Data.List as Builtin
import qualified Data.Char as Builtin
import qualified Data.String as Builtin

import HotAir.Byte (Byte)
import qualified HotAir.Byte as Byte
import HotAir.Function ((.))
import HotAir.List (List, cons, foldr, nil)
import HotAir.Semigroup (Semigroup)

newtype ByteString =
  ByteString (List Byte)
  deriving (Semigroup)

instance Builtin.IsString ByteString where
  fromString = ByteString . Builtin.foldr (cons . Byte.fromBuiltinIntegral . Builtin.ord) nil

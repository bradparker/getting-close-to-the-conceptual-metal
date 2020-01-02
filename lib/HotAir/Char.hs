{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Char
  ( Char,
    fromBuiltin,
    toBuiltin
    )
where

import qualified Data.Char as Builtin
import Data.Function ((.))
import Data.Functor ((<$>))
import Foreign (Storable (..), Ptr, castPtr)
import Data.Int (Int)
import HotAir.Eq (Eq)
import HotAir.Nat (Nat)
import qualified HotAir.Nat as Nat
import Overloaded.Chars (FromChar(fromChar))
import System.IO (IO)

newtype Char
  = Char Nat
  deriving (Eq)

fromBuiltin :: Builtin.Char -> Char
fromBuiltin = Char . Nat.fromNum . Builtin.ord

toBuiltin :: Char -> Builtin.Char
toBuiltin (Char c) = Builtin.chr (Nat.toNum c)

instance Storable Char where

  sizeOf :: Char -> Int
  sizeOf _ = 4

  alignment :: Char -> Int
  alignment _ = 4

  peek :: Ptr Char -> IO Char
  peek = (fromBuiltin <$>) . peek @Builtin.Char . castPtr

  poke :: Ptr Char -> Char -> IO ()
  poke ptr = poke @Builtin.Char (castPtr ptr) . toBuiltin

instance FromChar Char where
  fromChar = fromBuiltin

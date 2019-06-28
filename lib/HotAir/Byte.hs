{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module HotAir.Byte
  ( Byte
  , byte
  , toBuiltinNum
  , fromBuiltinIntegral
  ) where

import qualified Data.Eq as Builtin
import GHC.Num (Num((+), fromInteger))
import qualified GHC.Num as Builtin
import qualified GHC.Real as Builtin

import HotAir.Bit (Bit, toBool)
import qualified HotAir.Bit as Bit
import HotAir.Bool (ifThenElse)
import HotAir.Function ((.))
import HotAir.List (List, cons, foldr, nil)

newtype Byte =
  Byte (forall c. (Bit -> Bit -> Bit -> Bit-> Bit -> Bit -> Bit -> Bit -> c) -> c)

byte :: Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Byte
byte _1 _2 _3 _4 _5 _6 _7 _8 = Byte (\b -> b _1 _2 _3 _4 _5 _6 _7 _8)

toList :: Byte -> List Bit
toList (Byte b) = b
  (\_1 _2 _3 _4 _5 _6 _7 _8 ->
    _1
      `cons` _2
      `cons` _3
      `cons` _4
      `cons` _5
      `cons` _6
      `cons` _7
      `cons` _8
      `cons` nil
  )

toBuiltinNum :: Builtin.Num a => Byte -> a
toBuiltinNum =
  foldr (\bit n -> if toBool bit then 1 + n + n else n + n) 0 . toList

fromBuiltinIntegral :: (Builtin.Integral a, Builtin.Eq a) => a -> Byte
fromBuiltinIntegral n =
  let (n1, _1) = Builtin.divMod n 2
      (n2, _2) = Builtin.divMod n1 2
      (n3, _3) = Builtin.divMod n2 2
      (n4, _4) = Builtin.divMod n3 2
      (n5, _5) = Builtin.divMod n4 2
      (n6, _6) = Builtin.divMod n5 2
      (n7, _7) = Builtin.divMod n6 2
      (n8, _8) = Builtin.divMod n7 2
  in  byte (Bit.fromBuiltinNum _1)
           (Bit.fromBuiltinNum _2)
           (Bit.fromBuiltinNum _3)
           (Bit.fromBuiltinNum _4)
           (Bit.fromBuiltinNum _5)
           (Bit.fromBuiltinNum _6)
           (Bit.fromBuiltinNum _7)
           (Bit.fromBuiltinNum _8)

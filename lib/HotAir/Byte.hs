{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module HotAir.Byte
  ( Byte
  , byte
  , toBuiltinNum
  ) where

import GHC.Num ((+), fromInteger)
import qualified GHC.Num as Builtin
import HotAir.Bit (Bit, toBool)
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

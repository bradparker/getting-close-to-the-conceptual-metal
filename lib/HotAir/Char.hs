{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Chars #-}
module HotAir.Char
  ( Char,
    fromBuiltin,
    toBuiltin,
    digitToNat,
    natToDigit,
    isDigit
    )
where

import qualified Data.Char as Builtin
import Data.Foldable (asum)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Foreign (Ptr, Storable (..), castPtr)
import GHC.Exts (fromListN)
import GHC.Num (Num (fromInteger))
import HotAir.Bool (Bool, false, ifThenElse, (||))
import HotAir.Eq (Eq ((==)))
import HotAir.List (List, foldr)
import HotAir.Maybe (Maybe, just, nothing)
import HotAir.Nat (Nat)
import qualified HotAir.Nat as Nat
import Overloaded.Chars (FromChar (fromChar))
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

isDigit :: Char -> Bool
isDigit c =
  foldr (\a acc -> a == c || acc) false
    ( [ '0',
        '1',
        '2',
        '3',
        '4',
        '5',
        '6',
        '7',
        '8',
        '9'
        ]
        :: List Char
      )

digitToNat :: Char -> Maybe Nat
digitToNat c =
  asum
    ( [ if c == '0' then just 0 else nothing,
        if c == '1' then just 1 else nothing,
        if c == '2' then just 2 else nothing,
        if c == '3' then just 3 else nothing,
        if c == '4' then just 4 else nothing,
        if c == '5' then just 5 else nothing,
        if c == '6' then just 6 else nothing,
        if c == '7' then just 7 else nothing,
        if c == '8' then just 8 else nothing,
        if c == '9' then just 9 else nothing
        ]
        :: List (Maybe Nat)
      )

natToDigit :: Nat -> Maybe Char
natToDigit c =
  asum
    ( [ if c == 0 then just '0' else nothing,
        if c == 1 then just '1' else nothing,
        if c == 2 then just '2' else nothing,
        if c == 3 then just '3' else nothing,
        if c == 4 then just '4' else nothing,
        if c == 5 then just '5' else nothing,
        if c == 6 then just '6' else nothing,
        if c == 7 then just '7' else nothing,
        if c == 8 then just '8' else nothing,
        if c == 9 then just '9' else nothing
        ]
        :: List (Maybe Char)
      )

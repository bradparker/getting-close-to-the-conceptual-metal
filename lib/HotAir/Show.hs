{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Chars #-}
module HotAir.Show
  ( Show (show)
    )
where

import Data.Function (($), (.))
import Data.Semigroup ((<>))
import Data.String (fromString)
import GHC.Num (Num (fromInteger))
import HotAir.Bool (Bool, ifThenElse)
import HotAir.Char (Char, natToDigit)
import HotAir.Eq (Eq ((==)))
import HotAir.List (List, concatMap, cons, drop, reverse, unfoldr)
import HotAir.Maybe (Maybe, fromMaybe, just, maybe, nothing)
import HotAir.Nat (Nat, divMod)
import HotAir.Pair (Pair, fst, pair, snd)
import HotAir.String (String)
import qualified HotAir.String as String

class Show a where

  show :: a -> String

instance Show Char where
  show c = "'" <> String.singleton c <> "'"

instance Show String where
  show a = "\"" <> a <> "\""

instance Show a => Show (List a) where
  show as =
    "["
      <> String.fromList
           ( drop 1
               ( concatMap
                   ((',' `cons`) . String.toList . show)
                   as
                 )
             )
      <> "]"

instance Show Bool where
  show b = ifThenElse b "true" "false"

instance (Show a, Show b) => Show (Pair a b) where
  show p = "(pair " <> show (fst p) <> " " <> show (snd p) <> ")"

instance Show a => Show (Maybe a) where
  show = maybe "nothing" $ \a -> "(just " <> show a <> ")"

instance Show Nat where
  show =
    String.fromList
      . reverse
      . unfoldr
          ( \n ->
              let res = divMod n 10
               in if n == 0
                    then nothing
                    else
                      just
                        ( pair
                            (fromMaybe '0' (natToDigit (snd res)))
                            (fst res)
                          )
            )

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Chars #-}
module Main
  ( main
    )
where

import Control.Applicative ((*>), (<*), (<*>), (<|>), empty, pure)
import Control.Monad ((=<<))
import Data.Function (($), (.), id)
import Data.Functor ((<$), (<$>), fmap)
import Data.Traversable (traverse)
import GHC.Num ((*), (+))
import HotAir.Char (digitToNat)
import HotAir.IO (IO, print, putStrLn)
import HotAir.List (foldr, reverse)
import HotAir.Maybe (Maybe, maybe)
import HotAir.Nat (Nat)
import HotAir.Parser (Parser, char, digits, execParser)
import HotAir.String (String)
import qualified HotAir.String as String

apply :: (a -> b -> c) -> a -> b -> c
apply = id

readNat :: String -> Maybe Nat
readNat =
  fmap (foldr (\n acc -> acc * 10 + n) 0 . reverse)
    . traverse digitToNat
    . String.toList

eval :: String -> Maybe Nat
eval =
  execParser
    ( apply
        <$> (char '(' *> parseOp <* char ' ')
        <*> (parseNat <* char ' ')
        <*> (parseNat <* char ')')
      )
  where
    parseOp :: Parser (Nat -> Nat -> Nat)
    parseOp = (+) <$ char '+' <|> (*) <$ char '*'
    parseNat :: Parser Nat
    parseNat = maybe empty pure . readNat =<< digits

main :: IO ()
main = do
  putStrLn "Evaluating (+ 12 11) ..."
  print $ eval "(+ 12 11)"
  putStrLn "Evaluating (* 12 11) ..."
  print $ eval "(* 12 11)"

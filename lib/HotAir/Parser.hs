{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Parser
  ( Parser (parse),
    string,
    satisfy,
    char
    )
where

import Control.Applicative (Alternative ((<|>), empty), Applicative ((<*>), pure))
import Control.Monad (Monad (..))
import Data.Function ((.))
import Data.Functor ((<$>), Functor (fmap))
import Data.Traversable (traverse)
import HotAir.Bool (Bool, ifThenElse)
import HotAir.Char (Char)
import HotAir.Eq ((==))
import HotAir.List (uncons)
import HotAir.Maybe (Maybe, just, maybe, nothing)
import HotAir.Pair (Pair, fst, pair, snd)
import HotAir.String (String, fromList, toList)

newtype Parser a
  = Parser
      { parse :: String -> Maybe (Pair a String)
        }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parser =
    Parser \input ->
      fmap
        (\result -> pair (f (fst result)) (snd result))
        (parse parser input)

instance Applicative Parser where

  pure :: a -> Parser a
  pure a = Parser \input -> just (pair a input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa =
    Parser \input -> do
      resF <- parse pf input
      let f = fst resF
          input' = snd resF
      resA <- parse pa input'
      let a = fst resA
          input'' = snd resA
      pure (pair (f a) input'')

instance Alternative Parser where

  empty :: Parser a
  empty = Parser \_ -> nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  pa <|> pb =
    Parser \input ->
      maybe (parse pb input) just (parse pa input)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred =
  Parser \input -> do
    ht <- uncons (toList input)
    if pred (fst ht)
      then pure (pair (fst ht) (fromList (snd ht)))
      else nothing

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string = (fromList <$>) . traverse char . toList

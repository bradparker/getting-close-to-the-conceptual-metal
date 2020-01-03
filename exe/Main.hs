{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS -fplugin=Overloaded #-}
{-# OPTIONS -fplugin-opt=Overloaded:Chars #-}
module Main
  ( main
    )
where

import Control.Applicative ((*>), (<*), (<*>), (<|>))
import Data.Function (($), id)
import Data.Functor ((<$), (<$>))
import GHC.Num ((*), (+))
import HotAir.IO (IO, print, putStrLn)
import HotAir.Maybe (Maybe)
import HotAir.Nat (Nat)
import HotAir.Parser (Parser, char, execParser, nat)
import HotAir.String (String)

eval :: String -> Maybe Nat
eval = execParser expr
  where
    expr :: Parser Nat
    expr =
      nat <|> apply
        <$> (char '(' *> op <* char ' ')
        <*> (expr <* char ' ')
        <*> (expr <* char ')')
    op :: Parser (Nat -> Nat -> Nat)
    op = (+) <$ char '+' <|> (*) <$ char '*'
    apply :: (a -> b -> c) -> a -> b -> c
    apply = id

main :: IO ()
main = do
  putStrLn "Evaluating (+ 12 11) ..."
  print $ eval "(+ 12 11)"
  putStrLn "Evaluating (* (+ 12 11) 11) ..."
  print $ eval "(* (+ 12 11) 11)"

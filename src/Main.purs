module Main where

import Prelude
import Data.Array (head, drop)
import Data.List
import Data.String (toCharArray, contains)
import Data.Char (toString)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Node.Process as Process

data Token =
  LParenTok
  | RParenTok
  | LambdaTok
  | DotTok
  | NameTok Char

tokenize :: String -> List Token
tokenize = tokenize' <<< toList <<< toCharArray

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

tokenize' :: List Char -> List Token
tokenize' (Cons '(' rest) = Cons LParenTok $ tokenize' rest
tokenize' (Cons ')' rest) = Cons RParenTok $ tokenize' rest
tokenize' (Cons '.' rest) = Cons DotTok $ tokenize' rest
tokenize' (Cons '\\' rest) = Cons LambdaTok $ tokenize' rest
tokenize' (Cons c rest) =
  let
    remaining = tokenize' rest
  in
    if contains (toString c) alphabet then (Cons (NameTok c) remaining) else remaining
tokenize' Nil = Nil

main = do
  code <- head <<< drop 2 <$> liftEff Process.argv
  log $ "Running: " ++ show code
  log "Hello sailor!"

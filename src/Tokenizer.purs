module Tokenizer (tokenize) where

import Prelude (($), (<<<))
import Data.Char (toString)
import Data.List (List(..), toList)
import Data.String (toCharArray, contains)

import Token (Token(..))

tokenize :: String -> List Token
tokenize = tokenize' <<< toList <<< toCharArray

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

isLowerAlpha :: Char -> Boolean
isLowerAlpha c = contains (toString c) alphabet

tokenize' :: List Char -> List Token
tokenize' (Cons '(' rest) = Cons LParenTok $ tokenize' rest
tokenize' (Cons ')' rest) = Cons RParenTok $ tokenize' rest
tokenize' (Cons '.' rest) = Cons DotTok $ tokenize' rest
tokenize' (Cons '\\' rest) = Cons LambdaTok $ tokenize' rest
tokenize' (Cons c rest) =
  let
    remaining = tokenize' rest
  in
    if isLowerAlpha c then (Cons (NameTok c) remaining) else remaining
tokenize' Nil = Nil

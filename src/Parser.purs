module Parser (parse) where

import Prelude (($), (<<<))
import Data.List (List(..), drop)
import Data.Tuple (Tuple(..), fst, snd)

import Ast (Ast(..))
import Token (Token(..))

parse :: List Token -> Ast
parse = fst <<< parseSingle

parseSingle :: List Token -> Tuple Ast (List Token)
parseSingle (Cons (NameTok name) rest) = Tuple (Name name) rest
-- TODO: Use functor
parseSingle (Cons LambdaTok (Cons (NameTok arg) (Cons DotTok rest))) =
  let
    body = parseSingle rest
  in
    Tuple (Lambda arg $ fst body) (snd body)
parseSingle (Cons LParenTok applicationBody) =
  let
    result1 = parseSingle applicationBody
    result2 = parseSingle $ snd result1
  in
    Tuple (Application (fst result1) (fst result2)) (drop 1 (snd result2))

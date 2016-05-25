module Main where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Array (head)
import Data.Array as Array
import Data.Char (toString)
import Data.List (List(..), toList, drop)
import Data.Maybe (maybe)
import Data.Maybe.Unsafe (fromJust)
import Data.String (toCharArray, contains)
import Data.Tuple (Tuple(..), fst, snd, lookup)
import Node.Process as Process

data Ast =
 Application Ast Ast
 | Name Char
 | Lambda Char Ast
 | Closure Char Ast (List (Tuple Char Ast))

interp :: String -> String
interp = tokenize >>> parse >>> evaluate >>> pretty

pretty :: Ast -> String
pretty (Application fn arg) = "(" ++ pretty fn ++ " " ++ pretty arg ++ ")"
pretty (Name name) = toString name
pretty (Lambda arg body) = "\\" ++ toString arg ++ "." ++ pretty body
pretty (Closure arg body _) = "\\" ++ toString arg ++ "." ++ pretty body

evaluate :: Ast -> Ast
evaluate = evalInEnv Nil

evalInEnv :: List (Tuple Char Ast) -> Ast -> Ast
evalInEnv env (Name name) = fromJust $ lookup name env
evalInEnv env (Lambda arg body) = Closure arg body env
evalInEnv env (Application fn arg) =
  let
    closure = evalInEnv env fn
    value = evalInEnv env arg
  in
    case closure of
      (Closure arg body closedEnv) -> evalInEnv (Cons (Tuple arg value) (closedEnv ++ env)) body

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

main = do
  code <- head <<< Array.drop 2 <$> liftEff Process.argv
  log $ maybe "" interp code

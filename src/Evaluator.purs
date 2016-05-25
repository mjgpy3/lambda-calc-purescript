module Evaluator (evaluate) where

import Prelude (($), (++))
import Data.List (List(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(..), lookup)

import Ast (Ast(..))

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

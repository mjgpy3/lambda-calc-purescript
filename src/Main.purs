module Main where

import Prelude ((>>>), (<$>), bind, ($))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Array (head, drop)
import Data.Maybe (maybe)
import Node.Process as Process

import Evaluator (evaluate)
import Parser (parse)
import PrettyPrinter (pretty)
import Tokenizer (tokenize)

interp :: String -> String
interp = tokenize >>> parse >>> evaluate >>> pretty

main = do
  code <- drop 2 >>> head <$> liftEff Process.argv
  log $ maybe "" interp code

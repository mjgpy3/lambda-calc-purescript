module Ast (Ast(..)) where

import Data.List (List)
import Data.Tuple (Tuple)

data Ast =
 Application Ast Ast
 | Name Char
 | Lambda Char Ast
 | Closure Char Ast (List (Tuple Char Ast))

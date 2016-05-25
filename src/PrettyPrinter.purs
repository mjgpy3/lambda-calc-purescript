module PrettyPrinter (pretty) where

import Prelude ((++))
import Data.Char (toString)

import Ast (Ast(..))

pretty :: Ast -> String
pretty (Application fn arg) = "(" ++ pretty fn ++ " " ++ pretty arg ++ ")"
pretty (Name name) = toString name
pretty (Lambda arg body) = "\\" ++ toString arg ++ "." ++ pretty body
pretty (Closure arg body _) = "\\" ++ toString arg ++ "." ++ pretty body

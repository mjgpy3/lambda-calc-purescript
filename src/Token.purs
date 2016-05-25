module Token (Token(..)) where

data Token =
  LParenTok
  | RParenTok
  | LambdaTok
  | DotTok
  | NameTok Char


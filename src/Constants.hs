module Constants (keywords, symbol, precedence, Operator (..)) where

keywords :: [String]
keywords = ["in", "let", "fun", "if", "then", "else"]

data Operator = Plus | Minus | Mul | Div deriving (Eq)

symbol :: Operator -> String
symbol Plus = "+"
symbol Minus = "-"
symbol Mul = "*"
symbol Div = "/"

precedence :: Operator -> Int
precedence Plus = 0
precedence Minus = 0
precedence Mul = 1
precedence Div = 1

instance Show Operator where
  show = symbol
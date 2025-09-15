{-# LANGUAGE LambdaCase #-}

module Parser (parse, ExprE (..), PatternE (..)) where

import Constants
import Control.Monad
import Data.Char (isAlphaNum, isSpace)
import Data.Functor
import Data.List (dropWhileEnd)
import Data.Maybe (listToMaybe)
import GHC.OldList (intercalate)
import Prelude hiding ((+))

newtype Parser a = Pars {run :: String -> [(a, String)]}

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Pars (\s -> [(a, s)])
  (<*>) = ap

instance Monad Parser where
  return = pure
  p >>= f = Pars (\cs -> concat [run (f a) cs' | (a, cs') <- run p cs])

(<+>) :: Parser a -> Parser a -> Parser a
p1 <+> p2 = Pars (\s -> run p1 s ++ run p2 s)

(<++>) :: Parser a -> Parser a -> Parser a
p1 <++> p2 = Pars (\s -> let r = run p1 s in if null r then run p2 s else r)

zero :: Parser a
zero = Pars $ const []

next :: Parser Char
next =
  Pars
    ( \case
        x : xs -> [(x, xs)]
        _ -> []
    )

peek :: Parser Char
peek =
  Pars
    ( \case
        x : xs -> [(x, x : xs)]
        _ -> []
    )

match :: (Char -> Bool) -> Parser Char
match f = do
  a <- next
  if f a then return a else zero

token :: Char -> Parser Char
token c = match (c ==)

greedyMany :: Parser a -> Parser [a]
greedyMany p =
  ( do
      x <- p
      xs <- greedyMany p
      return $ x : xs
  )
    <++> return []

many :: Parser a -> Parser [a]
many p =
  return []
    <+> ( do
            x <- p
            xs <- many p
            return $ x : xs
        )

data PatternE = VarPatternE String | TuplePatternE [PatternE]

data ExprE = LetE PatternE ExprE ExprE | FunE [PatternE] ExprE | VarE String | IfThenElseE ExprE ExprE ExprE | TupleE [ExprE] | AppE [ExprE] | OpE Operator | ParenthesizedE ExprE

instance Show PatternE where
  show (VarPatternE v) = v
  show (TuplePatternE vars) = "(" ++ intercalate "," (map show vars) ++ ")"

instance Show ExprE where
  show (LetE p val e) = "let " ++ show p ++ " = " ++ show val ++ " in " ++ show e
  show (FunE pats e) = "fun " ++ unwords (map show pats) ++ " => " ++ show e
  show (VarE v) = v
  show (IfThenElseE i t e) = "if " ++ show i ++ " then " ++ show t ++ " else " ++ show e
  show (TupleE exprs) = "(" ++ intercalate "," (map show exprs) ++ ",)"
  show (AppE exprs) = "[" ++ unwords (map show exprs) ++ "]"
  show (OpE op) = show op
  show (ParenthesizedE e) = "(" ++ show e ++ ")"

operator :: Parser Operator
operator =
  match (`elem` ['+', '-', '*', '/'])
    <&> \case
      '+' -> Plus
      '-' -> Minus
      '*' -> Mul
      '/' -> Div
      _ -> error "Unreachable: Non-existant operator"

parenthesized :: Parser b -> Parser b
parenthesized p = do
  _ <- token '('
  _ <- whitespace
  inner <- p
  _ <- whitespace
  _ <- token ')'
  return inner

csl :: Parser a -> Parser [a]
csl p =
  return []
    <+> ( do
            x <- p
            xs <- many (whitespace >>= const (token ',') >>= const whitespace >>= const p)
            _ <- whitespace
            trailingComma <- peek
            when (trailingComma == ',') $ do
              _ <- next
              return ()
            return $ x : xs
        )

isFinished :: Parser Bool
isFinished = Pars (\x -> if null x then [(True, x)] else [(False, x)])

untilNext :: (Char -> Bool) -> Parser String
untilNext f = do
  finished <- isFinished
  if finished
    then return ""
    else do
      x <- peek
      if f x
        then return ""
        else do
          _ <- next
          xs <- untilNext f
          return $ x : xs

whitespace :: Parser [Char]
whitespace = untilNext (not . isSpace)

word :: Parser String
word = do
  x <- peek
  if isSpace x
    then zero
    else untilNext (\c -> isSpace c || c `elem` "(),")

alphaNumString :: Parser String
alphaNumString = do
  x <- peek
  if not $ isAlphaNum x
    then zero
    else untilNext (not . isAlphaNum)

name :: Parser String
name = do
  x <- alphaNumString
  if x `elem` ["in", "let", "fun", "if", "then", "else"]
    then zero
    else return x

matchWord :: String -> Parser String
matchWord x = do
  w <- word
  if w == x then return w else zero

nonEmptyWsl :: Parser a -> Parser [a]
nonEmptyWsl p = do
  x <- p
  xs <- greedyMany (whitespace >>= const p)
  return $ x : xs

wsl :: Parser a -> Parser [a]
wsl p =
  return [] <+> nonEmptyWsl p

parenList :: Parser a -> Parser [a]
parenList = parenthesized . csl

pat :: Parser PatternE
pat = do
  x <- peek
  if x == '('
    then do
      TuplePatternE <$> parenList pat
    else do
      VarPatternE <$> (name <+> matchWord "_")

tupleE :: Parser ExprE
tupleE = TupleE <$> parenList expr

fun :: Parser ExprE
fun = do
  _ <- matchWord "fun"
  _ <- whitespace
  patterns <- wsl pat
  _ <- whitespace
  _ <- matchWord "=>"
  _ <- whitespace
  FunE patterns <$> expr

varE :: Parser ExprE
varE = VarE <$> name

letE :: Parser ExprE
letE = do
  _ <- matchWord "let"
  _ <- whitespace
  p <- pat
  _ <- whitespace
  _ <- matchWord "="
  _ <- whitespace
  v <- expr
  _ <- whitespace
  _ <- matchWord "in"
  _ <- whitespace
  LetE p v <$> expr

ifthenelseE :: Parser ExprE
ifthenelseE = do
  _ <- matchWord "if"
  _ <- whitespace
  i <- expr
  _ <- whitespace
  _ <- matchWord "then"
  _ <- whitespace
  t <- expr
  _ <- whitespace
  _ <- matchWord "else"
  _ <- whitespace
  IfThenElseE i t <$> expr

appE :: Parser ExprE
appE = AppE <$> nonEmptyWsl innerExprE

parenthesizedE :: Parser ExprE
parenthesizedE = ParenthesizedE <$> parenthesized expr

operatorE :: Parser ExprE
operatorE = OpE <$> operator

innerExprE :: Parser ExprE
innerExprE = do
  x <- peek
  if x == '('
    then parenthesizedE <++> tupleE
    else
      if isAlphaNum x
        then fun <++> letE <++> ifthenelseE <++> varE
        else operatorE

expr :: Parser ExprE
expr = do
  e <- appE
  case e of
    AppE [x] -> return x
    _ -> return e

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parse :: String -> Maybe ExprE
parse s = listToMaybe [x | (x, rest) <- run expr (trim s), null rest]
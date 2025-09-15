module AST (refine, Expr (..), Pattern (..)) where

import Constants (Operator, precedence)
import Data.List (intercalate)
import Parser
import Text.Read (readMaybe)

data Pattern
  = TuplePattern [Pattern]
  | VarPattern String
  | Wildcard

data Expr
  = App Expr Expr
  | BuiltinOp Operator
  | Var String
  | NumVal Int
  | BoolVal Bool
  | Let Pattern Expr Expr
  | IfThenElse Expr Expr Expr
  | Fun [Pattern] Expr
  | Tuple [Expr]

instance Show Pattern where
  show (TuplePattern ps) = "[" ++ intercalate ", " (map show ps) ++ "]"
  show (VarPattern var) = var
  show Wildcard = "_"

instance Show Expr where
  show (App (App (BuiltinOp op) e1) e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (BuiltinOp op) = show op
  show (Var v) = v
  show (NumVal n) = show n
  show (BoolVal b) = show b
  show (Let p v e) = "let " ++ show p ++ " = " ++ show v ++ " in " ++ show e
  show (IfThenElse i t e) = "if " ++ show i ++ " then " ++ show t ++ " else " ++ show e
  show (Fun ps e) = "fun " ++ unwords (map show ps) ++ " => " ++ show e
  show (Tuple es) = "[" ++ intercalate ", " (map show es) ++ "]"

-- decide the order of operations in an application considering operator precedence
refineApp :: [ExprE] -> Expr -> Int -> (Expr, [ExprE])
refineApp [OpE op] e1 _ = (App e1 $ refine (OpE op), [])
refineApp (OpE op : x : xs) e1 prec =
  if prec < precedence op
    then
      let (e2, rest') = refineApp xs (refine x) (precedence op)
       in refineApp rest' (App (App (refine $ OpE op) e1) e2) prec
    else (e1, OpE op : x : xs)
refineApp (x : xs) e1 prec =
  refineApp xs (App e1 (refine x)) prec
refineApp [] e _ = (e, [])

refinePattern :: PatternE -> Pattern
refinePattern (VarPatternE v) = VarPattern v
refinePattern (TuplePatternE ps) = TuplePattern $ map refinePattern ps

refine :: ExprE -> Expr
refine (OpE op) = BuiltinOp op
refine (AppE (e : es)) = fst $ refineApp es (refine e) (-1)
refine (AppE []) = error "Empty application is illegal"
refine (LetE p v e) = Let (refinePattern p) (refine v) (refine e)
refine (IfThenElseE i t e) = IfThenElse (refine i) (refine t) (refine e)
refine (FunE ps e) = Fun (map refinePattern ps) (refine e)
refine (VarE v) =
  case v of
    "true" -> BoolVal True
    "false" -> BoolVal False
    _ -> case readMaybe v :: Maybe Int of
      Just n -> NumVal n
      Nothing -> Var v
refine (TupleE es) = Tuple (map refine es)
refine (ParenthesizedE e) = refine e
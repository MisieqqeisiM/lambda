module ExTerm (ExTerm (..), compile) where

import AST (Expr (..), Pattern (..))
import Constants
import Control.Monad (forM_)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.List (elemIndex)
import Data.Map hiding (foldl, foldr)

data ExTerm = ExAbs ExTerm | ExVar Int | ExApp ExTerm ExTerm | ExNumber Int | ExOp Operator | ExTuple [ExTerm] | ExIth Int
  deriving (Eq, Show)

data CompilationState = CS {unboundVars :: [String], boundVars :: [String]}

getVar :: String -> StateT CompilationState Identity ExTerm
getVar v = do
  CS unbound bound <- get
  case elemIndex v bound of
    Just idx -> return $ ExVar idx
    Nothing -> case elemIndex v unbound of
      Just idx -> return $ ExVar (-1 - idx)
      Nothing -> do
        put $ CS (v : unbound) bound
        return $ ExVar (-length unbound)

bindVar :: String -> StateT CompilationState Identity ()
bindVar v = do
  CS unbound bound <- get
  put $ CS unbound (v : bound)

unbindVar :: StateT CompilationState Identity ()
unbindVar = do
  CS unbound bound <- get
  case bound of
    (_ : bs) -> put $ CS unbound bs
    [] -> error "No bound variable to unbind"

ithChain :: [Int] -> ExTerm
ithChain = foldr (ExApp . ExIth) (ExVar 0)

getBindings :: Pattern -> [Int] -> Map String ExTerm
getBindings (TuplePattern pats) chain =
  let maps = zipWith (\p i -> getBindings p (i : chain)) pats [0 ..]
   in unions maps
getBindings (VarPattern name) chain =
  singleton name (ithChain chain)
getBindings Wildcard _ = empty

withArgs :: [ExTerm] -> ExTerm -> ExTerm
withArgs as body = foldl (ExApp . ExAbs) body as

compile' :: Expr -> StateT CompilationState Identity ExTerm
compile' (App e1 e2) = do
  e1' <- compile' e1
  e2' <- compile' e2
  return $ ExApp e1' e2'
compile' (Var v) = getVar v
compile' (NumVal n) = return $ ExNumber n
compile' (BoolVal b) = return $ if b then ExIth 1 else ExIth 0
compile' (Let p v e) = compile' (App (Fun [p] e) v)
compile' (IfThenElse cond true false) = do
  cond' <- compile' cond
  true' <- compile' $ Fun [Wildcard] true
  false' <- compile' $ Fun [Wildcard] false
  return $ ExApp (ExApp cond' (ExTuple [false', true'])) (ExTuple [])
compile' (Fun [] e) = compile' $ Fun [Wildcard] e
compile' (Fun [Wildcard] e) = do
  bindVar ""
  inner <- compile' e
  unbindVar
  return $ ExAbs inner
compile' (Fun [VarPattern name] e) = do
  bindVar name
  inner <- compile' e
  unbindVar
  return $ ExAbs inner
compile' (Fun [p] e) = do
  let bindings = toList $ getBindings p []
  mapM_ (bindVar . fst) bindings
  inner <- compile' e
  forM_ bindings (const unbindVar)
  return $ withArgs (snd <$> bindings) inner
compile' (Fun (p : ps) e) = compile' $ Fun [p] (Fun ps e)
compile' (Tuple es) = do
  es' <- mapM compile' es
  return $ ExTuple es'
compile' (BuiltinOp o) = return $ ExOp o

compile :: Expr -> ExTerm
compile e = evalState (compile' e) (CS [] [])
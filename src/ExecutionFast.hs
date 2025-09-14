module ExecutionFast (test) where

import Control.Monad.State
import Data.Map (Map, empty, fromList, insert, lookup, union)

data Term = Abs Int | App Int Int | UnboundVar Int | Subs (Map Int Int) Int deriving (Eq, Show)

data ExTerm = ExAbs ExTerm | ExVar Int | ExApp ExTerm ExTerm

data ExecutionState = ES {cache :: Map Int Term, idx :: Int} deriving (Show)

getTerm' :: Int -> State ExecutionState Term
getTerm' i = do
  es <- get
  case Data.Map.lookup i (cache es) of
    Nothing -> error "No such term >:("
    Just t -> return t

getSub :: Int -> Map Int Int -> State ExecutionState Int
getSub i subs = do
  t <- getTerm' i
  case t of
    UnboundVar v -> case Data.Map.lookup v subs of
      Nothing -> return i
      Just i' -> getSub i' subs
    _ -> return i

getTerm :: Int -> Map Int Int -> State ExecutionState Term
getTerm i subs = do
  t <- getTerm' i
  case t of
    UnboundVar v -> case Data.Map.lookup v subs of
      Nothing -> return t
      Just i' -> getTerm i' subs
    _ -> return t

showTerm :: Int -> Map Int Int -> State ExecutionState String
showTerm i subs = do
  i' <- getSub i subs
  t <- getTerm' i'
  case t of
    Abs a -> do
      as <- showTerm a subs
      return $ "fun $(" ++ show i' ++ ") => " ++ as
    UnboundVar v -> return $ "$(" ++ show v ++ ")"
    App a b -> do
      as <- showTerm a subs
      bs <- showTerm b subs
      return $ "(" ++ as ++ ")(" ++ bs ++ ")"
    Subs subs' a -> do
      let allSubs = Data.Map.union subs' subs
      showTerm a allSubs

setTerm :: Int -> Term -> State ExecutionState ()
setTerm i t = do
  es <- get
  put $ ES (insert i t $ cache es) (idx es)

newTerm :: Term -> State ExecutionState Int
newTerm t = do
  es <- get
  let i = idx es
  put $ ES (insert i t $ cache es) (i + 1)
  return i

load :: ExTerm -> [Int] -> State ExecutionState Int
load (ExAbs t) vars = do
  tmp <- newTerm (Abs 0)
  t' <- load t (tmp : vars)
  setTerm tmp (Abs t')
  return tmp
load (ExVar i) vars = do
  if i >= 0
    then
      newTerm $ UnboundVar (vars !! i)
    else
      newTerm $ UnboundVar i
load (ExApp a b) vars = do
  a' <- load a vars
  b' <- load b vars
  newTerm $ App a' b'

reduceTerm :: Int -> Map Int Int -> State ExecutionState Int
reduceTerm i subs = do
  t <- getTerm i subs
  case t of
    Abs _ -> return i
    UnboundVar _ -> return i
    App a b -> do
      b' <- reduceTerm b subs
      if b' /= b
        then newTerm $ App a b'
        else do
          a' <- getSub a subs
          at <- getTerm' a'
          case at of
            Abs inner -> do
              replaceVar a' b inner subs
            _ -> do
              a'' <- reduceTerm a' subs
              if a' == a then return i else newTerm $ App a'' b
    Subs subs' a -> do
      let allSubs = Data.Map.union subs' subs
      a' <- reduceTerm a allSubs
      if a' /= a then newTerm $ Subs allSubs a' else return i

replaceVar :: Int -> Int -> Int -> Map Int Int -> State ExecutionState Int
replaceVar from to inTerm _ = do
  newTerm $ Subs (Data.Map.fromList [(from, to)]) inTerm

reduceWithCount :: Int -> State ExecutionState (Int, Int)
reduceWithCount i = do
  i' <- reduceTerm i Data.Map.empty
  if i' == i
    then
      return (i, 0)
    else do
      (tree, steps) <- reduceWithCount i'
      return (tree, steps + 1)

test :: (String, ExecutionState)
test =
  let two = ExAbs $ ExAbs (ExApp (ExVar 1) (ExApp (ExVar 1) (ExVar 0)))
   in let expensive = ExApp (ExAbs $ ExApp (ExApp (ExApp (ExVar 0) (ExVar 0)) (ExVar (-1))) (ExVar (-2))) two
       in let suspended = ExAbs expensive
           in let twoExpensive = ExApp (ExAbs $ ExApp (ExApp (ExAbs (ExAbs (ExVar 0))) (ExApp (ExVar 0) (ExVar 0))) (ExApp (ExVar 0) (ExVar 0))) suspended
               in let state = ES Data.Map.empty 0
                   in runState
                        ( do
                            i <- load expensive []
                            i <- reduceTerm i Data.Map.empty
                            showTerm i Data.Map.empty
                        )
                        state
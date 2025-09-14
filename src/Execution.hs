{-# LANGUAGE EmptyCase #-}

module Execution (test) where

import Control.Monad (when)
import Control.Monad.State
import Data.Map (Map, delete, empty, insert, lookup)
import qualified Data.Maybe as Maybe
import qualified GHC.Base as Control.Monad

data Term = Abs Int | App Int Int | UnboundVar Int deriving (Eq, Show)

data ExTerm = ExAbs ExTerm | ExVar Int | ExApp ExTerm ExTerm

data ExecutionState = ES {cache :: Map Int (Int, Term), idx :: Int} deriving (Show)

getTermWithRc :: Int -> State ExecutionState (Int, Term)
getTermWithRc i = do
  es <- get
  case Data.Map.lookup i (cache es) of
    Nothing -> error "No such term >:("
    Just result -> return result

getTerm :: Int -> State ExecutionState Term
getTerm i = do
  es <- get
  case Data.Map.lookup i (cache es) of
    Nothing -> error "No such term >:("
    Just (_, t) -> return t

delTerm :: Int -> State ExecutionState ()
delTerm i = do
  es <- get
  t <- getTerm i
  put $ ES (Data.Map.delete i (cache es)) (idx es)
  case t of
    Abs a -> decRc a
    UnboundVar _ -> return ()
    App a b -> do
      decRc a
      decRc b

showTerm :: Int -> State ExecutionState String
showTerm i = do
  t <- getTerm i
  case t of
    Abs a -> do
      as <- showTerm a
      return $ "fun $(" ++ show i ++ ") => " ++ as
    UnboundVar v -> return $ "$(" ++ show v ++ ")"
    App a b -> do
      as <- showTerm a
      bs <- showTerm b
      return $ "(" ++ as ++ ")(" ++ bs ++ ")"

getRc :: Int -> State ExecutionState Int
getRc i = do
  es <- get
  case Data.Map.lookup i (cache es) of
    Just (rc', _) -> return rc'
    Nothing -> return 0

dropIfUnreferenced :: Int -> State ExecutionState ()
dropIfUnreferenced i = do
  rc <- getRc i
  when (rc <= 0) $ delTerm i

incRc :: Int -> State ExecutionState ()
incRc i = do
  es <- get
  (rc, t) <- getTermWithRc i
  put $ ES (insert i (rc + 1, t) $ cache es) (idx es)

decRc :: Int -> State ExecutionState ()
decRc i = do
  es <- get
  (rc, t) <- getTermWithRc i
  put $ ES (insert i (rc - 1, t) $ cache es) (idx es)
  if rc <= 1
    then delTerm i
    else do
      put $ ES (insert i (rc - 1, t) $ cache es) (idx es)

setTerm :: Int -> Term -> State ExecutionState ()
setTerm i t = do
  es <- get
  rc <- getRc i
  put $ ES (insert i (rc, t) $ cache es) (idx es)

newTerm :: Term -> State ExecutionState Int
newTerm t = do
  es <- get
  let i = idx es
  put $ ES (insert i (0, t) $ cache es) (i + 1)
  return i

load :: ExTerm -> [Int] -> State ExecutionState Int
load (ExAbs t) vars = do
  tmp <- newTerm (Abs 0)
  t' <- load t (tmp : vars)
  setTerm tmp (Abs t')
  incRc tmp
  return tmp
load (ExVar i) vars = do
  if i >= 0
    then do
      i' <- newTerm $ UnboundVar (vars !! i)
      incRc i'
      return i'
    else do
      i' <- newTerm $ UnboundVar i
      incRc i'
      return i'
load (ExApp a b) vars = do
  a' <- load a vars
  b' <- load b vars
  i <- newTerm $ App a' b'
  incRc i
  return i

reduceTerm' :: Int -> State ExecutionState Int
reduceTerm' i = do
  t <- getTerm i
  case t of
    Abs _ -> return i
    UnboundVar _ -> return i
    App a b -> do
      b' <- reduceTerm' b
      if b' /= b
        then do
          incRc a
          incRc b'
          newTerm $ App a b'
        else do
          at <- getTerm a
          case at of
            Abs inner -> do
              rc <- getRc a
              if rc == 1
                then
                  replaceVarInPlace a b inner
                else
                  replaceVar a b inner
            _ -> do
              a' <- reduceTerm' a
              if a' == a
                then return i
                else do
                  incRc a'
                  incRc b
                  newTerm $ App a' b

reduceTerm :: Int -> State ExecutionState Int
reduceTerm i = do
  i' <- reduceTerm' i
  incRc i'
  decRc i
  return i'

replaceVar :: Int -> Int -> Int -> State ExecutionState Int
replaceVar from to inTerm = do
  t <- getTerm inTerm
  case t of
    Abs i -> do
      i' <- replaceVar from to i
      let t' = Abs i'
      if t == t'
        then return inTerm
        else do
          tmp <- newTerm $ Abs 0
          var <- newTerm $ UnboundVar tmp
          i'' <- replaceVar inTerm var i
          i''' <- replaceVar from to i''
          setTerm tmp $ Abs i'''
          incRc i'''
          dropIfUnreferenced i'
          dropIfUnreferenced i''
          return tmp
    App a b -> do
      a' <- replaceVar from to a
      b' <- replaceVar from to b
      let t' = App a' b'
      if t == t'
        then return inTerm
        else do
          incRc a'
          incRc b'
          newTerm t'
    UnboundVar i -> do
      if i == from
        then do
          return to
        else
          return inTerm

replaceVarInPlace :: Int -> Int -> Int -> State ExecutionState Int
replaceVarInPlace from to inTerm = do
  t <- getTerm inTerm
  case t of
    Abs i -> do
      i' <- replaceVarInPlace from to i
      incRc i'
      decRc i
      setTerm inTerm $ Abs i'
      return inTerm
    App a b -> do
      a' <- replaceVarInPlace from to a
      b' <- replaceVarInPlace from to b
      incRc a'
      incRc b'
      decRc a
      decRc b
      setTerm inTerm $ App a' b'
      return inTerm
    UnboundVar i -> do
      if i == from
        then do
          return to
        else
          return inTerm

reduceWithCount :: Int -> State ExecutionState (Int, Int)
reduceWithCount i = do
  i' <- reduceTerm i
  if i' == i
    then
      return (i, 0)
    else do
      (tree, steps) <- reduceWithCount i'
      return (tree, steps + 1)

test :: (String, ExecutionState)
test =
  let two = ExAbs $ ExAbs (ExApp (ExVar 1) (ExApp (ExVar 1) (ExApp (ExVar 1) (ExVar 0))))
   in let expensive = ExApp (ExAbs $ ExApp (ExApp (ExApp (ExVar 0) (ExVar 0)) (ExVar (-1))) (ExVar (-2))) two
       in let suspended = ExAbs expensive
           in let twoExpensive = ExApp (ExAbs $ ExApp (ExApp (ExAbs (ExAbs (ExVar 0))) (ExApp (ExVar 0) (ExVar 0))) (ExApp (ExVar 0) (ExVar 0))) suspended
               in let state = ES Data.Map.empty 0
                   in runState
                        ( do
                            i <- load twoExpensive []
                            i <- reduceTerm i
                            i <- reduceTerm i
                            -- (i', steps) <- reduceWithCount i
                            text <- showTerm i
                            return text
                        )
                        -- \$ show steps ++ " " ++ text

                        state
module Execution (test) where

import Constants (Operator (..))
import Control.Monad (when)
import Control.Monad.State
import Data.List (intercalate)
import Data.Map (Map, delete, empty, insert, lookup)

data Term = Abs Int | App Int Int | UnboundVar Int | Number Int | Op Operator | Tuple [Int] | Ith Int
  deriving (Eq, Show)

data ExTerm = ExAbs ExTerm | ExVar Int | ExApp ExTerm ExTerm | ExNumber Int | ExOp Operator | ExTuple [ExTerm] | ExIth Int
  deriving (Eq, Show)

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
    App a b -> do
      decRc a
      decRc b
    _ -> return ()

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
    Number n -> return $ show n
    Op o -> return $ show o
    Tuple xs -> do
      xss <- mapM showTerm xs
      return $ "[" ++ intercalate ", " xss ++ "]"
    Ith idx -> do
      return $ "|" ++ show idx ++ "|"

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
load (ExNumber n) _ = do
  i <- newTerm $ Number n
  incRc i
  return i
load (ExOp o) _ = do
  i <- newTerm $ Op o
  incRc i
  return i
load (ExTuple xs) vars = do
  xs' <- mapM (`load` vars) xs
  i <- newTerm $ Tuple xs'
  incRc i
  return i
load (ExIth idx) _ = do
  i <- newTerm $ Ith idx
  incRc i
  return i

-- dupTerm :: Int -> Int -> State ExecutionState ()
-- dupTerm from to = do
--   fromT <- getTerm from
--   case fromT of
--     Abs a -> do
--       incRc a
--     App a b -> do
--       incRc a
--       incRc b
--     _ -> return ()
--   toT <- getTerm to
--   case toT of
--     Abs a -> do
--       decRc a
--     App a b -> do
--       decRc a
--       decRc b
--     _ -> return ()
--   setTerm to fromT

reduceTerm' :: Int -> State ExecutionState (Bool, Int)
reduceTerm' i = do
  t <- getTerm i
  case t of
    Abs _ -> return (False, i)
    UnboundVar _ -> return (False, i)
    App a b -> do
      (reducedB, b') <- reduceTerm' b
      if reducedB
        then do
          incRc a
          incRc b'
          result <- newTerm $ App a b'
          return (True, result)
        else do
          at <- getTerm a
          case at of
            Abs inner -> do
              rc <- getRc a
              varExists <- varExistsInTerm a inner
              if not varExists
                then do
                  return (True, inner)
                else
                  if rc == 1
                    then do
                      result <- replaceVarInPlace a b inner
                      return (True, result)
                    else do
                      result <- replaceVar a b inner
                      return (True, result)
            Ith idx -> do
              bt <- getTerm b
              case bt of
                Tuple xs -> do
                  if idx < length xs
                    then do
                      let chosen = xs !! idx
                      return (True, chosen)
                    else return (False, i)
                _ -> do
                  (reducedA, a') <- reduceTerm' a
                  if not reducedA
                    then return (False, i)
                    else do
                      incRc a'
                      incRc b
                      result <- newTerm $ App a' b
                      return (True, result)
            App c d -> do
              -- ((c d) b)
              ct <- getTerm c
              dt <- getTerm d
              bt <- getTerm b
              case (ct, dt, bt) of
                (Op op, Number n1, Number n2) -> case op of
                  Plus -> do
                    result <- newTerm $ Number (n1 + n2)
                    return (True, result)
                  Minus -> do
                    result <- newTerm $ Number (n1 - n2)
                    return (True, result)
                  Mul -> do
                    result <- newTerm $ Number (n1 * n2)
                    return (True, result)
                  Div -> do
                    result <- newTerm $ Number (n1 `div` n2)
                    return (True, result)
                _ -> do
                  (reducedA, a') <- reduceTerm' a
                  if not reducedA
                    then return (False, i)
                    else do
                      incRc a'
                      incRc b
                      result <- newTerm $ App a' b
                      return (True, result)
            _ -> do
              (reducedA, a') <- reduceTerm' a
              if not reducedA
                then return (False, i)
                else do
                  incRc a'
                  incRc b
                  result <- newTerm $ App a' b
                  return (True, result)
    _ -> return (False, i)

reduceTerm :: Int -> State ExecutionState (Bool, Int)
reduceTerm i = do
  (reduced, i') <- reduceTerm' i
  incRc i'
  decRc i
  return (reduced, i')

varExistsInTerm :: Int -> Int -> State ExecutionState Bool
varExistsInTerm v inTerm = do
  t <- getTerm inTerm
  case t of
    Abs a -> varExistsInTerm v a
    App a b -> do
      a' <- varExistsInTerm v a
      b' <- varExistsInTerm v b
      return $ a' || b'
    Tuple xs -> do
      ress <- mapM (varExistsInTerm v) xs
      return $ or ress
    UnboundVar i -> return $ i == v
    _ -> return False

replaceVar :: Int -> Int -> Int -> State ExecutionState Int
replaceVar from to inTerm = do
  t <- getTerm inTerm
  case t of
    Abs i -> do
      varExists <- varExistsInTerm inTerm i
      if not varExists
        then return inTerm
        else do
          tmp <- newTerm $ Abs 0
          var <- newTerm $ UnboundVar tmp
          i' <- replaceVar inTerm var i
          i'' <- replaceVar from to i'
          setTerm tmp $ Abs i''
          incRc i''
          dropIfUnreferenced i'
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
    Tuple xs -> do
      ress <- mapM (replaceVar from to) xs
      if Tuple ress == t
        then return inTerm
        else do
          mapM_ incRc ress
          newTerm $ Tuple ress
    UnboundVar i -> do
      if i == from
        then do
          return to
        else
          return inTerm
    _ -> return inTerm

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
    Tuple xs -> do
      ress <- mapM (replaceVarInPlace from to) xs
      mapM_ incRc ress
      mapM_ decRc xs
      setTerm inTerm $ Tuple ress
      return inTerm
    UnboundVar i -> do
      if i == from
        then do
          return to
        else
          return inTerm
    _ -> return inTerm

reduceWithCount :: Int -> State ExecutionState (Int, Int)
reduceWithCount i = do
  (reduced, i') <- reduceTerm i
  if not reduced
    then
      return (i, 0)
    else do
      (tree, steps) <- reduceWithCount i'
      return (tree, steps + 1)

test :: (String, ExecutionState)
test =
  let addTwo = ExAbs $ ExTuple [ExNumber 2, ExVar 0]
   in let two = ExAbs $ ExAbs (ExApp (ExVar 1) (ExApp (ExVar 1) (ExApp (ExVar 1) (ExVar 0))))
       in let expensive = ExApp (ExIth 7) (two `ExApp` addTwo `ExApp` ExNumber 1)
           in --      in let suspended = ExAbs expensive
              --          in let twoExpensive = ExApp (ExAbs $ ExApp (ExApp (ExAbs (ExAbs (ExVar 0))) (ExApp (ExVar 0) (ExVar 0))) (ExApp (ExVar 0) (ExVar 0))) suspended
              let state = ES Data.Map.empty 0
               in runState
                    ( do
                        i <- load expensive []

                        (i', steps) <- reduceWithCount i
                        text <- showTerm i'
                        return $ show steps ++ " " ++ text
                    )
                    state
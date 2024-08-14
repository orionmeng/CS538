{-# LANGUAGE InstanceSigs #-}

module Eval (
  runEval,
  TermEnv,
  emptyTmenv,
  Value(..)
) where

import Syntax

import Control.Monad.Identity ( Identity(runIdentity) )
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure [(Pattern, Expr)] TermEnv
  | VArray [Value]

type TermEnv = Map.Map String Value
type Interpreter t = Identity t

instance MonadFail Identity where
  fail :: String -> Identity a
  fail = error

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

instance Show Value where
  show :: Value -> String
  show (VInt n) = show n
  show (VBool n) = show n
  show (VClosure{}) = "<<closure>>"
  show (VArray vals) = "[" ++ (concatMap show vals) ++ "]"

checkeq :: Lit -> Value -> Bool
checkeq (LInt litInt) (VInt valInt) = litInt == valInt
checkeq (LBool litBool) (VBool valBool) = litBool == valBool
checkeq _ _ = False

match :: [(Pattern, Expr)] -> Value -> (Expr, TermEnv)
match ((pat, body):rest) val = case pat of
  PVar v -> (body, Map.singleton v val)

  PLit lit ->
    if checkeq lit val then (body, Map.empty)
    else match rest val

  PCons x xs ->
    case val of
      VArray (head : tail) ->
        let newEnv = Map.fromList [(x, head), (xs, VArray tail)]
        in (body, newEnv)
      _ -> match rest val
match [] _ = error "error: no pattern matched"

eval :: TermEnv -> Expr -> Interpreter Value
eval env expr = case expr of
  Lit (LInt k)  -> return $ VInt k
  Lit (LBool k) -> return $ VBool k
  Lit (LArray elements) -> do
    evaluatedElements <- mapM (eval env) elements
    return $ VArray evaluatedElements

  Var x -> do
    let Just v = Map.lookup x env
    return v

  Op Cons a b -> do
      a' <- eval env a
      b' <- eval env b
      case b' of
        VArray list -> return $ VArray (a' : list)
        _ -> error "error: expected list"

  Op Concat a b -> do
    a' <- eval env a
    b' <- eval env b
    case (a', b') of
      (VArray list1, VArray list2) -> return $ VArray (list1 ++ list2)
      _ -> error "error: expected lists"

  Op op a b -> do
    VInt a' <- eval env a
    VInt b' <- eval env b
    return $ binop op a' b'

  Lam x body ->
    return (VClosure [(x, body)] env)

  App fun arg -> do
    funVal <- eval env fun
    argVal <- eval env arg

    case funVal of
      VClosure patterns clo -> do
        let (matchedBody, newEnv) = match patterns argVal
        let combinedEnv = Map.union newEnv clo
        eval combinedEnv matchedBody
      _ -> error "error: non-function value"

  Let x e body -> do
    e' <- eval env e
    let nenv = Map.insert x e' env
    eval nenv body

  If cond tr fl -> do
    VBool br <- eval env cond
    if br
    then eval env tr
    else eval env fl

  Fix e -> do
    eval env (App e (Fix e))

binop :: Binop -> Integer -> Integer -> Value
binop Add a b = VInt $ a + b
binop Mul a b = VInt $ a * b
binop Sub a b = VInt $ a - b
binop Eql a b = VBool $ a == b

runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex =
  let (result, newEnv) = runIdentity $ do
        res <- eval env ex
        let newEnv = Map.insert nm res env
        return (res, newEnv)
  in
  (result, newEnv)

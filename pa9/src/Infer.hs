{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Infer where

import Prelude hiding (foldr)

import Type
import Syntax

import Control.Monad.State
    ( MonadState(put, get), foldM, replicateM, evalState, State )
import Control.Monad.Except
    ( MonadError(throwError), foldM, replicateM, runExceptT, ExceptT )

import Data.List (nub)
import Data.Foldable (foldr)
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype TypeEnv = TypeEnv (Map.Map Var Scheme)
  deriving (Monoid, Semigroup)

newtype Unique = Unique { count :: Int }

type Infer = ExceptT TypeError (State Unique)
type Subst = Map.Map TVar Type

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique { count = 0 }

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> Var -> Maybe Type.Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply :: Subst -> Type -> Type
  apply _ (TCon a)       = TCon a
  apply s t@(TVar a)     = Map.findWithDefault t a s
  apply s (t1 `TArrow` t2) = apply s t1 `TArrow` apply s t2
  apply s (TArray t) = TArray (apply s t)

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArrow` t2) = ftv t1 `Set.union` ftv t2
  ftv (TArray t) = ftv t

instance Substitutable Scheme where
  apply :: Subst -> Scheme -> Scheme
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv :: Scheme -> Set.Set TVar
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply :: Substitutable a => Subst -> [a] -> [a]
  apply = fmap . apply
  ftv :: Substitutable a => [a] -> Set.Set TVar
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply :: Subst -> TypeEnv -> TypeEnv
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv :: TypeEnv -> Set.Set TVar
  ftv (TypeEnv env) = ftv $ Map.elems env


nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

unify :: Type -> Type -> Infer Subst
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return nullSubst
unify (TArray t1) (TArray t2) = do
  s <- unify t1 t2
  return s
unify (TArray t1) t2 = throwError $ UnificationFail (TArray t1) t2
unify t1 (TArray t2) = do
  s <- unify t1 t2
  return s
unify (t1 `TArrow` t2) (t3 `TArrow` t4) = do
  s1 <- unify t1 t3
  s2 <- unify (apply s1 t2) t4
  return (s2 `compose` s1)
unify t1 t2 = throwError $ UnificationFail t1 t2

bind ::  TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = return $ Map.singleton a t

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Binop -> Type
ops Add = typeInt `TArrow` typeInt `TArrow` typeInt
ops Mul = typeInt `TArrow` typeInt `TArrow` typeInt
ops Sub = typeInt `TArrow` typeInt `TArrow` typeInt
ops Eql = typeInt `TArrow` typeInt `TArrow` typeBool
ops Cons = TVar (TV "a") `TArrow` TArray (TVar (TV "a")) `TArrow` TArray (TVar (TV "a"))
ops Concat = TArray (TVar (TV "a")) `TArrow` TArray (TVar (TV "a")) `TArrow` TArray (TVar (TV "a"))

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

extendEnvWithPattern :: TypeEnv -> Pattern -> Type -> Infer (TypeEnv, Type)
extendEnvWithPattern env pat ty = case pat of
  PVar v -> do
    let newEnv = extend env (v, Forall [] ty)
    return (newEnv, ty)

  PLit (LInt _) -> return (env, typeInt)
  PLit (LBool _) -> return (env, typeBool)

  PCons x xs -> do
    headType <- fresh
    tailType <- fresh
    let newEnv = extend env (x, Forall [] headType)
    let finalEnv = extend newEnv (xs, Forall [] tailType)
    return (finalEnv, tailType)

  _ -> throwError $ UnificationFail ty ty

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of

  Var x -> lookupEnv env x

  Lam pat e -> do
    tv <- fresh
    (newEnv, _) <- extendEnvWithPattern env pat tv
    (s1, t1) <- infer newEnv e
    let funcType = apply s1 tv `TArrow` t1
    return (s1, funcType)

  App e1 e2 -> do
    retType <- fresh
    (s1, funcType) <- infer env e1
    (s2, argType) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 funcType) (TArrow argType retType)
    return (s3 `compose` s2 `compose` s1, apply s3 retType)

  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let genScheme = generalize (apply s1 env) t1
    let newEnv = extend (apply s1 env) (x, genScheme)
    (s2, t2) <- infer newEnv e2
    return (s2 `compose` s1, t2)

  If cond tr fl -> do
    tv <- fresh
    inferPrim env [cond, tr, fl] (typeBool `TArrow` tv `TArrow` tv `TArrow` tv)

  Fix e1 -> do
    tv <- fresh
    inferPrim env [e1] ((tv `TArrow` tv) `TArrow` tv)

  Op Cons e1 e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    let expectedTailType = TArray t1
    s3 <- unify t2 expectedTailType
    let appliedType = apply s3 (TArray t1)
    return (s3 `compose` s2 `compose` s1, appliedType)

  Op Concat e1 e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    case (t1, t2) of
      (TArray t1', TArray t2') -> do
        s3 <- unify t1' t2'
        let appliedType = apply s3 (TArray t1')
        return (s3 `compose` s2 `compose` s1, appliedType)
      _ -> throwError (UnificationFail t1 t2)

  Op op e1 e2 -> do
    inferPrim env [e1, e2] (ops op)

  Lit (LInt _)  -> return (nullSubst, typeInt)
  Lit (LBool _) -> return (nullSubst, typeBool)
  Lit (LArray exprs) -> do
    (s, elemTypes) <- foldM inferArrayElement (nullSubst, []) exprs
    let arrayType = foldr (TArrow . TArray) (TCon "Array") elemTypes
    return (s, apply s arrayType)
    where
      inferArrayElement :: (Subst, [Type]) -> Expr -> Infer (Subst, [Type])
      inferArrayElement (s, elemTypes) expr = do
        (s', elemType) <- infer (apply s env) expr
        let s'' = s' `compose` s
        return (s'', elemType : elemTypes)

inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
  inferStep (s, tf) exp = do
    (s', t) <- infer (apply s env) exp
    return (s' `compose` s, tf . TArrow t)

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = do
  ty <- inferExpr env ex
  inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TVar a)   = [a]
    fv (TArrow a b) = fv a ++ fv b
    fv (TCon _)   = []
    fv (TArray t) = fv t

    normtype (TArrow a b) = TArrow (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"
    normtype (TArray t) = TArray (normtype t)

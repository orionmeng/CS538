module Syntax where

type Var = String

data Expr
  = Var Var
  | App Expr Expr
  | Lam Pattern Expr
  | Let Var Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  | LArray [Expr]
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql | Cons | Concat
  deriving (Eq, Ord, Show)

data Pattern
  = PVar Var
  | PCons Var Var
  | PLit Lit
  deriving (Show, Eq, Ord)

type Decl = (String, Expr)

data Program = Program [Decl] Expr deriving (Show, Eq)

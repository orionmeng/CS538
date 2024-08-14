{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseExpr,
  parseModule
) where

import Text.Parsec
    ( optional, (<|>), many, many1, parse, try, sepBy, ParseError, choice, optionMaybe )
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L

import Lexer
import Syntax

integer :: Parser Integer
integer = Tok.natural lexer

variable :: Parser Expr
variable = do
  Var <$> identifier

number :: (Lit -> a) -> Parser a
number c = do
  c . LInt . fromIntegral <$> integer

bool :: (Lit -> a) -> Parser a
bool c = (reserved "True" >> return (c (LBool True)))
    <|> (reserved "False" >> return (c (LBool False)))

list :: (Lit -> a) -> Parser Expr
list c = do
  elements <- Tok.brackets lexer (expr `sepBy` Tok.comma lexer)
  return (Lit (LArray elements))

pattern :: Parser Pattern
pattern = try (reservedOp ":" >> do
                   head <- identifier
                   tail <- identifier
                   return (PCons head tail))
      <|> (identifier >>= return . PVar)
      <|> (integer >>= return . PLit . LInt)
      <|> (reserved "True" >> return (PLit (LBool True)))
      <|> (reserved "False" >> return (PLit (LBool False)))

patternToVar :: Pattern -> String
patternToVar (PVar x) = x
patternToVar _ = error "error: expected pattern"

fix :: Parser Expr
fix = do
  reservedOp "fix"
  Fix <$> expr

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many pattern
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)

letrecin :: Parser Expr
letrecin = do
  reserved "let"
  reserved "rec"
  p <- pattern
  reservedOp "="
  e1 <- expr
  reserved "in"
  Let (patternToVar p) e1 <$> expr

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  If cond tr <$> expr

aexp :: Parser Expr
aexp =
      parens expr
  <|> list Lit
  <|> bool Lit
  <|> number Lit
  <|> ifthen
  <|> fix
  <|> try letrecin
  <|> letin
  <|> lambda
  <|> variable

term :: Parser Expr
term = aexp >>= \x ->
                (many1 aexp >>= \xs -> return (foldl App x xs))
                <|> return x

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

table :: Operators Expr
table = [
    [
      infixOp "*" (Op Mul) Ex.AssocLeft
    ],
    [
      infixOp "+" (Op Add) Ex.AssocLeft
    , infixOp "-" (Op Sub) Ex.AssocLeft
    ],
    [
      infixOp "==" (Op Eql) Ex.AssocLeft
    ],
    [
      infixOp ":" (Op Cons) Ex.AssocRight
    ],
    [
      infixOp "++" (Op Concat) Ex.AssocRight
    ]
  ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

type Binding = (String, Expr)

letdecl :: Parser Binding
letdecl = do
  reserved "let"
  name <- identifier
  args <- many pattern
  reservedOp "="
  body <- expr
  return (name, foldr (Lam) body args)

letrecdecl :: Parser (String, Expr)
letrecdecl = do
  reserved "let"
  reserved "rec"
  name <- identifier
  args <- many pattern
  reservedOp "="
  body <- expr
  return (name, Fix $ foldr Lam body args)

val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

decl :: Parser Binding
decl = try letrecdecl <|> letdecl <|> val

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseModule ::  FilePath -> L.Text -> Either ParseError [(String, Expr)]
parseModule = parse (contents modl)

module Expr where 

import qualified Data.Map.Strict as M 

data BinOp = Plus | Mult | Minus | Div | Pow deriving (Show, Eq)
data UnOp = Sqrt deriving (Show, Eq)

data Expr a
  = Num a
  | Var String
  | BinOp BinOp (Expr a) (Expr a)
  | UnOp UnOp (Expr a)
  deriving (Show, Eq)

instance Num a => Num (Expr a) where
  a + b = BinOp Plus a b
  a - b = BinOp Minus a b
  a * b = BinOp Mult a b
  abs = error "abs not implemented"
  signum = error "signum not implemented"
  fromInteger i = Num (fromInteger i)
  negate = BinOp Mult (Num (-1))
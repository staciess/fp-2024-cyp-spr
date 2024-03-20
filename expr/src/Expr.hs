module Expr where
import Text.Printf (printf)

data Expr
  = Expr Double
  | Var String
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  | Expr :^: Expr
  | Sqr Expr
  deriving Eq


instance Show Expr where
  show (Expr x) = show x
  show (Var v)  = v
  show (a :+: b) = printf "(%s + %s)" (show a) (show b)
  show (a :-: b) = printf "(%s - %s)" (show a) (show b)
  show (a :*: b) = printf "(%s * %s)" (show a) (show b)
  show (a :/: b) = printf "(%s / %s)" (show a) (show b)
  show (a :^: b) = printf "(%s ^ %s)" (show a) (show b)
  show (Sqr x)   = printf "sqrt(%s)" $ show x

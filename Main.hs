module Main where

import Text.Printf (printf)
import Control.Monad (unless)
import Distribution.Simple.Utils (xargs)
import Language.Haskell.TH (Exp)

data Expr
  = Expr Double
  | (:+) Expr Expr
  | (:-) Expr Expr
  | (:*) Expr Expr
  | (:/) Expr Expr
  | (:^) Expr Expr
  | Sqr  Expr
  deriving Eq


instance Show Expr where
  show (Expr x) = show x
  show ((:+) a b)  = printf "(%a + %b)" (show a) (show b)
  show ((:-) a b) = printf  "(%a - %b)" (show a) (show b)
  show ((:*) a b) = printf  "%(a * %b)" (show a) (show b)
  show ((:/) a b) = printf  "%(a / %b)" (show a) (show b)
  show ((:^) a b) = printf  "%(a ^ %b)" (show a) (show b)
  show (Sqr x) = printf "sqrt(%s)" $ show x

data Error
  = NegativeSqr Expr
  | NegativePwr Expr
  | DivisionByZero Expr

instance Show Error where
  show (NegativeSqr expr) = printf "Negative square root"
  show (DivisionByZero expr) = printf "Division by zero"

instance Eq Error where
  (NegativeSqr expr1) == (NegativeSqr expr2) = expr1 == expr2
  (NegativePwr expr1) == (NegativePwr expr2) = expr1 == expr2
  (DivisionByZero expr1) == (DivisionByZero expr2) = expr1 == expr2
  _ == _ = False


eval :: Expr -> Either Error Double
eval expr = case expr of
  Expr x      -> Right x
  (:+) a b    -> binOp (+) a b
  (:-) a b    -> binOp (-) a b
  (:*) a b    -> binOp (*) a b
  (:/) a b    -> if b == Expr 0 then Left (DivisionByZero a) else binOp (/) a b
  (:^) a b    -> binOp (**) a b
  Sqr x       -> evalSqr x
  where
    evalSqr :: Expr -> Either Error Double
    evalSqr (Expr a)
      | a < 0.0   = Left (NegativeSqr (Expr a))
      | otherwise = Right (sqrt a)
    evalSqr e    = eval (Sqr e)

binOp :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
binOp f a b = do
  v <- eval a
  w <- eval b
  return (f v w)


cases :: [(Expr, Either Error Double)]
cases =
  [ (Expr 5, Right 5)
  , (Sqr (Expr (-5)), Left (NegativeSqr (Expr (-5))))
  , (Expr 10 :+ Expr 5, Right 15)
  , (Expr 10 :- Expr 5, Right 5)
  , (Expr 10 :* Expr 5, Right 50)
  , (Expr 10 :/ Expr 5, Right 2)
  , (Expr 10 :/ Expr 0, Left (DivisionByZero (Expr 10)))
  , (Expr 2 :^ Expr 3, Right 8)
  , (Sqr (Expr 25), Right 5)
  , (Sqr (Expr (-25)), Left (NegativeSqr (Expr (-25))))
  ]

test :: Expr -> Either Error Double -> IO ()
test expr expected =
    let actual = eval expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)


main :: IO ()
main = mapM_ (uncurry test) cases

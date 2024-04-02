{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Expr where
import StateDemo ( State, execState, get, modify )
import Data.Maybe ( fromJust )
import Text.Printf (printf)
import Control.Monad.State

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

data Error
  = NegativeSqr Expr
  | NegativePwr Expr
  | DivisionByZero Expr
  deriving Eq

instance Show Error where
  show (NegativeSqr a) = printf "Negative square root"
  show (NegativePwr a) = printf "Negative power"
  show (DivisionByZero a) = printf "Division by zero"

type ExprState = [(String, Double)]

eval :: Expr -> State ExprState (Either Error Double)
eval expr = case expr of
  Expr x       -> return $ Right x
  Var v        -> do
    env <- get
    case lookup v env of
      Just x -> return $ Right x
      Nothing -> return $ Left $ UnknownVariable v
  (a :+: b)    -> evalBinOp (+) a b
  (a :-: b)    -> evalBinOp (-) a b
  (a :*: b)    -> evalBinOp (*) a b
  (a :/: b)    -> evalDiv a b
  (a :^: b)    -> evalBinOp (**) a b
  Sqr x        -> evalSqr x
  where
    evalBinOp f a b = do
      x <- eval a
      y <- eval b
      case (x, y) of
        (Right x', Right y') -> return $ Right (f x' y')
        (Left e, _) -> return $ Left e
        (_, Left e) -> return $ Left e
    evalDiv a b = do
      x <- eval a
      y <- eval b
      case (x, y) of
        (Right x', Right y')
          | y' == 0 -> return $ Left (DivisionByZero b)
          | otherwise -> return $ Right (x' / y')
        (Left e, _) -> return $ Left e
        (_, Left e) -> return $ Left e
    evalSqr e = do
      x <- eval e
      case x of
        Right x' 
          | x' < 0 -> return $ Left (NegativeSqr e)
          | otherwise -> return $ Right (sqrt x')
        Left e -> return $ Left e

runEval :: ExprState -> Expr -> Either EvalError Double
runEval s expr = evalState (eval expr) s

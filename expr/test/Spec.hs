module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Lib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [evalTests]

evalTests :: TestTree
evalTests = testGroup "Eval Tests"
  [ testCase "Evaluate simple expression" $
      eval (Expr 5) @?= Right 5
  , testCase "Evaluate square root of negative number" $
      eval (Sqr (Expr (-5))) @?= Left (NegativeSqr (Expr (-5)))
  , testCase "Evaluate addition" $
      eval (Expr 10 :+: Expr 5) @?= Right 15
  , testCase "Evaluate subtraction" $
      eval (Expr 10 :-: Expr 5) @?= Right 5
  , testCase "Evaluate multiplication" $
      eval (Expr 10 :*: Expr 5) @?= Right 50
  , testCase "Evaluate division" $
      eval (Expr 10 :/: Expr 5) @?= Right 2
  , testCase "Evaluate division by zero" $
      eval (Expr 10 :/: Expr 0) @?= Left (DivisionByZero (Expr 10))
  , testCase "Evaluate exponentiation" $
      eval (Expr 2 :^: Expr 3) @?= Right 8
  , testCase "Evaluate square root of positive number" $
      eval (Sqr (Expr 25)) @?= Right 5
  , testCase "Evaluate square root of negative number" $
      eval (Sqr (Expr (-25))) @?= Left (NegativeSqr (Expr (-25)))
  ]
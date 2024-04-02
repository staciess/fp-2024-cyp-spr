import Test.Tasty
import Test.Tasty.HUnit
import Expr (parseExpr, Expr(..))
import Parser

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

tests :: TestTree
tests = testGroup "Parser Tests"
  [ testCase "Positive integer" $
      parseExpr "123" @?= Just (Expr 123)
  , testCase "Variable" $
      parseExpr "xyz" @?= Just (Var "xyz")
  , testCase "Square root expr" $
      parseExpr "sqrt 123" @?= Just (Sqr (Expr 123))
  , testCase "Addition" $
      parseExpr "+ 123 45" @?= Just (Expr 123 :+: Expr 45)
  , testCase "Difference" $
      parseExpr "- 123 45" @?= Just (Expr 123 :-: Expr 45)
  , testCase "Multiplications" $
      parseExpr "* xyz 123" @?= Just (Var "xyz" :*: Expr 123)
  , testCase "Division" $
      parseExpr "\\ xyz sqr" @?= Just (Expr 0.0 :/: Sqr (Var "xyz"))
  , testCase "Expr test 1" $
      parseExpr "+ 123 * 45 6" @?= Just (Expr 123 :+: (Expr 45 :*: Expr 6))
  , testCase "Expr test 2" $
      parseExpr "+ * 123 45 6" @?= Just ((Expr 123 :*: Expr 45) :+: Expr 6)
  , testCase "Sqrt without an expr" $
      parseExpr "sqrt" @?= Nothing
  , testCase "Negatives" $
      parseExpr "- 123" @?= Nothing
  ]


import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure )
import Expr ( Expr (..), BinOp (..) )
import Eval ( eval )
import qualified Data.Map.Strict as M 

testEval :: TestTree
testEval = 
    testGroup "Eval" [ testAdd, testPow ]
  where 
    testEvalNoVarSuccess msg expr res = 
       testCase msg $ eval expr M.empty @?= Right res 
    testAdd = testGroup "Add" 
      [ testCase "1 + 2 == 3" $ eval (BinOp Plus (Num 1) (Num 2)) M.empty @?= Right 3
      , testCase "2 + 1 == 3" $ eval (BinOp Plus (Num 2) (Num 1)) M.empty @?= Right 3
      , testCase "2 + 1 == 3 as Num instance" $ eval (2+1) M.empty @?= Right 3
      , testEvalNoVarSuccess "0+2 == 2" (0+2) 2
      ]
    testPow = testGroup "Pow" 
      [ -- testEvalNoVarSuccess "(-4) ** 0.5 == NaN" (BinOp Pow (Num (-4)) (Num (0.5))) ((-4) ** 0.5) -- always fail due to NaN /= NaN
        testNaN "(-4) ** 0.5 == NaN" (BinOp Pow (Num (-4)) (Num (0.5)))
      ]
    testNaN msg expr = testCase msg $ 
      case eval expr M.empty of 
        Right x -> assertBool "Should be NaN" (isNaN x)
        Left x -> assertFailure $ "Evaluation produced an error " ++ show x 
    


main :: IO ()
main = 
  defaultMain $ testGroup "Expressions" [ testEval ]
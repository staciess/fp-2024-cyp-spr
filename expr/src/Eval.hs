module Eval ( eval ) where 

import Expr ( Expr (..), BinOp (..), UnOp (..) )
import Error ( Error (..) )

import qualified Data.Map.Strict as M

eval :: (Ord a, Floating a) => Expr a -> M.Map String a -> Either (Error a) a
eval (Num d) _ = Right d 
eval (Var v) subst = maybe (Left $ VarNotDefined v) Right (v `M.lookup` subst)
eval (UnOp Sqrt e) subst =
    either Left runSqrt (eval e subst)
  where 
    runSqrt v | v < 0 = Left (NegativeSqrt e)
              | otherwise = Right $ sqrt v 
eval (BinOp op x y) subst =
    let x' = eval x subst 
        y' = eval y subst 
    in  either Left (\xv -> either Left (runBinOp xv) y') x'
  where 
    runBinOp xv yv = 
      case op of 
        Plus -> Right $ xv + yv 
        Mult -> Right $ xv * yv 
        Minus -> Right $ xv - yv 
        Pow -> Right $ xv ** yv 
        Div | yv == 0 -> Left $ DivisionByZero y 
            | otherwise -> Right $ xv / yv 
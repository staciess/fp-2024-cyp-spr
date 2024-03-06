module Simplify ( simplify ) where 

import Expr ( Expr (..), BinOp (..), UnOp (..) )

simplify :: (Eq a, Num a) => Expr a -> Expr a
simplify (BinOp op x y) =
  let x' = simplify x
      y' = simplify y
  in  case (op, x', y') of
        (Mult, Num 1, _) -> y'
        (Mult, _, Num 1) -> x'
        (Mult, Num 0, _) -> Num 0
        (Mult, _, Num 0) -> Num 0
        (Div, _, Num 1) -> x'
        (Plus, _, Num 0) -> x'
        (Plus, Num 0, _) -> y'
        (Minus, _, Num 0) -> x'
        (Minus, _, _) | x' == y' -> Num 0
        _ -> BinOp op x' y'
simplify (UnOp op x) =  
  case (op, simplify x) of 
    (Sqrt, Num 0) -> Num 0 
    (Sqrt, Num 1) -> Num 1
    (_, x') -> UnOp Sqrt x' 
simplify x = x

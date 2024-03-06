module Error where 

import Expr ( Expr (..) ) 

data Error a 
  = VarNotDefined String 
  | DivisionByZero (Expr a)
  | NegativeSqrt (Expr a)
  deriving (Show, Eq)

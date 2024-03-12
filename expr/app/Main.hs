module Main(main) where

import Text.Printf (printf)
import Control.Monad (unless)
import Lib

cases :: [(Expr, Either Error Double)]
cases =
  [ (Expr 5, Right 5)
  , (Sqr (Expr (-5)), Left (NegativeSqr (Expr (-5))))
  , (Expr 10 :+: Expr 5, Right 15)
  , (Expr 10 :-: Expr 5, Right 5)
  , (Expr 10 :*: Expr 5, Right 50)
  , (Expr 10 :/: Expr 5, Right 2)
  , (Expr 10 :/: Expr 0, Left (DivisionByZero (Expr 10)))
  , (Expr 2 :^: Expr 3, Right 8)
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
main = do
  mapM_ (uncurry test) cases 
module Main where

import Text.Printf (printf)
import Control.Monad (unless)

data Expr a
  = Expr Double
  | Var String
  | (:+) Expr Expr
  | (:-) Expr Expr
  | (:*) Expr Expr
  | (:/) Expr Expr
  | (:^) Expr Expr
  | Sqr  Expr
  deriving Eq

instance Show Expr where
  show (Expr x) = show x
  show (Var v)  = v
  show ((:+) a b) = printf "(%a + %b)" (show a) (show b)
  show ((:-) a b) = printf  "(%a - %b)" (show a) (show b)
  show ((:*) a b) = printf  "%(a * %b)" (show a) (show b)
  show ((:/) a b) = printf  "%(a / %b)" (show a) (show b)
  show ((:^) a b) = printf  "%(a ^ %b)" (show a) (show b)
  show (Sqr x) = printf "sqrt(%s)" $ show x

data Error
  = NegativeSqr Expr
  | NegativePwr Expr
  | DivisionByZero Expr
  deriving Eq

instance Show Error where
  show (NegativeSqr) = printf "Negative square root"
  show (NegativePwr) = printf "Negative power"
  show (DivisionByZero) = printf "Division by zero"


eval :: Expr -> Either Error Double
eval expr = case expr of
  Expr x       -> Right x
  (:+) a b     -> binOp (+) a b
  (:-) a b     -> binOp (-) a b
  (:*) a b     -> binOp (*) a b
  (:/) a b     -> if b == Expr 0 then Left (DivisionByZero a) else binOp (/) a b
  (:^) a b     -> binOp (**) a b
  Sqr x        -> evalSqr x
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

data newEither a b = newLeft a | newRight b

instance Functor (newEither a) where
    fmap f (newRight x) = newRight (f x)
    fmap _ (newLeft x)  = newLeft x

{-
Left identity: fmap id = id
fmap id (newRight x) = newRight (id x) 
                     = newRight x

fmap id (newLeft x)  = newLeft x


Right identity: fmap (g . f) = fmap g . fmap f
fmap (g . f) (newRight x) = newRight ((g . f) x)
                          = newRight (g (f x))

(fmap g . fmap f) (newRight x) = fmap g (fmap f (newRight x))
                               = fmap g (newRight (f x))
                              = newRight (g (f x))

For Left x:
fmap (g . f) (newLeft x)  = newLeft x = (fmap g . fmap f) (newLeft x)
-}

instance Functor ((->) a) where
    fmap f g = f . g

{-
Left identity: fmap id = id
fmap id g = id . g
          = g

Right identity: fmap (g . f) = fmap g . fmap f
fmap (g . f) h = (g . f) . h
               = g . (f . h)

(fmap g . fmap f) h = fmap g (fmap f h)
                    = fmap g (f . h)
                    = g . (f . h)
-}

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
main = do
  mapM_ (uncurry test) cases
module Eval where

import Expr 
import Error 

eval :: Expr -> Either Error Double
eval expr = case expr of
  Expr x       -> Right x
  (a :+: b)    -> binOp (+) a b
  (a :-: b)    -> binOp (-) a b
  (a :*: b)    -> binOp (*) a b
  (a :/: b)    -> if b == Expr 0 then Left (DivisionByZero a) else binOp (/) a b
  (a :^: b)    -> binOp (**) a b
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

data NewEither a b = NewLeft a | NewRight b

instance Functor (NewEither a) where
    fmap f (NewRight x) = NewRight (f x)
    fmap _ (NewLeft x)  = NewLeft x

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

newtype MyArrow a b = MyArrow((->) a b)

instance Functor (MyArrow f) where
    fmap f (MyArrow g) = MyArrow (f . g)

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
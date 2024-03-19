module Either where 

data MyEither a b 
  = MyLeft a 
  | MyRight b 
  deriving (Show, Eq)

instance Functor (MyEither a) where
  fmap _ (MyLeft a) = MyLeft a
  fmap f (MyRight b) = MyRight (f b)

{-
fmap id (MyLeft a) = MyLeft a
fmap id (MyRight b) = MyRight (id b) = MyRight b

fmap (f . g) (MyLeft a) = MyLeft a = fmap f (MyLeft a) = fmap f (fmap g (MyLeft a)) = fmap f . fmap g (MyLeft a)
fmap (f . g) (MyRight b) = MyRight (f (g b)) = fmap f (MyRight (g b)) = fmap f (fmap g (MyRight b)) = fmap f . fmap g (MyRight b)
-}

instance Applicative (MyEither a) where
  pure = MyRight
  (MyLeft a) <*> _ = MyLeft a
  (MyRight b) <*> r = fmap b r

{-
pure id <*> (MyLeft a) = MyLeft a
pure id <*> (MyRight b) = MyRight (id b) = MyRight b

pure f <*> pure r = MyRight (f r)

MyLeft a <*> pure y = MyLeft a = pure ($ y) <*> MyLeft a
MyRight f <*> pure y = MyRight (f y) = pure ($ y) <*> MyRight f

pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-}
  
instance Monad (MyEither a) where
  return = pure
  (MyRight x) >>= f = f x
  (MyLeft a) >>= _ = MyLeft a
  
{-
return a >>= f = MyRight a >>= f = f a  --lft
MyRight x >>= return = return x = MyRight x 
MyLeft a >>= return = MyLeft a  --right

(m >>= f) >>= g = (MyEither >>= f) >>= g = m >>= (\x -> f x >>= g)
-}

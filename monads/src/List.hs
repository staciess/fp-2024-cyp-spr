module List where
import Data.List.NonEmpty (appendList)

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

{-
fmap id Nil = Nil = id Nil
fmap id (Cons x xs) = Cons (id x) (fmap id xs) = Cons x (fmap id xs) = id (Cons xs xs)

fmap (f . g) Nil = Nil = fmap f Nil = fmap f (fmap g Nil) = fmap f . fmap g Nil
fmap (f . g) (Cons x xs) = Cons (f (g x)) (fmap f (fmap g xs)) = fmap f (Cons (g x) (fmap g xs)) = fmap f . fmap g (Cons x xs)
-}

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = append (fmap f xs) (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

{-
pure id <*> Nil = Nil
pure id <*> (Cons x xs) = Cons x (pure id <*> xs) = Cons x xs

pure f <*> pure x = Cons f Nil <*> Cons x Nil = Cons (f x) Nil = pure (f x)

u <*> pure y = fmap (\f -> f y) u
  = case u of
    Nil -> Nil
    Cons f fs -> Cons (f y) (fs <*> pure y)
  = pure ($ y) <*> u

pure (.) <*> u <*> v <*> w = fmap (.) u <*> v <*> w
  = case u of
    Nil -> Nil <*> v <*> w
    Cons g gs -> Cons (.) (fmap (.) gs) <*> v <*> w
  = append (fmap (.) v) (fmap (\h -> h w) (fmap (.) u))
  = append (fmap (.) v) (append (fmap u v) (fmap (\h -> h w) u))
  = append (fmap (.) v) (append (append (fmap u v) (fmap u w)) Nil)
  = append (fmap (.) v) (fmap u (append (fmap v w) Nil))
  = fmap u (append (fmap v w) Nil)
  = fmap u (fmap v w)
  = u <*> (v <*> w)
-}

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = append (f x) (xs >>= f)

{-
return a >>= f = pure a >>= f = Cons a Nil >>= f = append (f a) Nil = f a

m >>= return = m >>= pure = append (pure a) (m' >>= pure) = append (pure a) m' = m

(m >>= f) >>= g
= (append (f x) (xs >>= f)) >>= g
= append (append (f x) (xs >>= f) >>= g) (xs >>= f >>= g)
= append (f x `append` (xs >>= f >>= g)) (xs >>= f >>= g)
= (Cons x xs >>= f) >>= g
-}

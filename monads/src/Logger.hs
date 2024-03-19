{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Logger where

data Logger l a = Logger [l] a deriving (Show, Eq)

instance Functor (Logger l) where
    fmap f (Logger logs x) = Logger logs (f x)

{-
fmap id (Logger logs x) = Logger logs (id x) = Logger logs x

fmap (g . h) (Logger logs x) = Logger logs ((g . h) x) = Logger logs (g (h x)) = fmap g (Logger logs (h x)) = fmap g (fmap h (Logger logs x)) = (fmap g . fmap h) (Logger logs x) 
-}

instance Applicative (Logger l) where
    Logger logs f <*> Logger logs' x = Logger (logs ++ logs') (f x)
    pure = Logger []

{-
pure id <*> Logger logs x = Logger [] id <*> Logger logs x = Logger ([]) (id x) = Logger lofs x

pure f <*> pure x = Logger [] f <*> Logger [] x = Logger [] (f x) = pure (f x)

Logger logs f <*> pure y = Logger logs f <*> Logger [] y = Logger logs (f y) = Logger [] ($ y) <*> Logger logs f = pure ($ y) <*> Logger logs f
-}


instance Monad (Logger l) where
  Logger logs x >>= f = let Logger logs' y = f x in Logger (logs ++ logs') y
  return = Logger []

{-
return a >>= f = Logger [] a >>= f = let Logger logs' y = Logger logs' (f a)

Logger logs x >>= return = let Logger logs' y = return x in Logger (logs ++ logs') y = Logger logs x

(m >>= f) >>= g = (Logger logs x >>= f) >>= g = let Logger logs' y = f x in Logger (logs ++ logs') y >>= g = let Logger logs'' z = g y in Logger (logs ++ logs' ++ logs'') z = let Logger logs' y = f x in Logger (logs ++ logs') y >>= g = m >>= (\x -> f x >>= g)
-}

-- Writes a single log message. 
-- Can be easily bound together with other logging computations.
writeLog :: l -> Logger l ()
writeLog l = Logger [l] ()

-- Logs every intermediate result 
-- ghci> factLog 5
-- Logger [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120)] 120
factLog :: Int -> Logger (Int, Int) Int
factLog n
  | n <= 0 = do
      let res = 1
      writeLog (n, res)
      return res
  | otherwise = do
      prev <- factLog (n - 1)
      let res = n * prev
      writeLog (n, res)
      return res    

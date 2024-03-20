{-# OPTIONS_GHC -Wno-unused-matches #-}
module Error where

import Expr

import Text.Printf (printf)
import Control.Monad (Functor, fmap, unless) 
import qualified GHC.Base as Base hiding (Functor)

data Error
  = NegativeSqr Expr
  | NegativePwr Expr
  | DivisionByZero Expr
  deriving Eq

instance Show Error where
  show (NegativeSqr a) = printf "Negative square root"
  show (NegativePwr a) = printf "Negative power"
  show (DivisionByZero a) = printf "Division by zero"
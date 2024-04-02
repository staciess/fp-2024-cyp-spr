{-# HLINT ignore "Eta reduce" #-}
module Parser where

import Expr
import Text.Read (readMaybe)
import Data.List.Split (splitOn)

parseExpr :: String -> Maybe Expr
parseExpr = parse . words

parse :: [String] -> Maybe Expr
parse [] = Nothing
parse (x:xs)
  | isNumeric x = parseNumeric x xs
  | isVariable x = parseVariable x xs
  | isOperator x = parseOperator x xs
  | isSqrt x     = parseSqrt xs
  | otherwise    = Nothing

isNumeric :: String -> Bool
isNumeric str = case readMaybe str :: Maybe Double of
  Just _  -> True
  Nothing -> False

isVariable :: String -> Bool
isVariable str = all (`elem` (['a'..'z'] ++ ['A'..'Z'])) str

isOperator :: String -> Bool
isOperator str = str `elem` ["+", "-", "*", "/", "^"]

isSqrt :: String -> Bool
isSqrt str = str == "sqrt"

parseNumeric :: String -> [String] -> Maybe Expr
parseNumeric str [] = Just (Expr (read str))
parseNumeric _ _    = Nothing

parseVariable :: String -> [String] -> Maybe Expr
parseVariable str [] = Just (Var str)
parseVariable _ _    = Nothing

parseSqrt :: [String] -> Maybe Expr
parseSqrt []     = Nothing
parseSqrt (x:xs) = case parse xs of
  Just expr -> Just (Sqr expr)
  Nothing   -> Nothing

parseOperator :: String -> [String] -> Maybe Expr
parseOperator op xs = case splitOn [op] xs of
  [exp1, exp2] -> case (parse exp1, parse exp2) of
    (Just expr1, Just expr2) -> case op of
      "+" -> Just (expr1 :+: expr2)
      "-" -> Just (expr1 :-: expr2)
      "*" -> Just (expr1 :*: expr2)
      "/" -> Just (expr1 :/: expr2)
      "^" -> Just (expr1 :^: expr2)
      _   -> Nothing
    _ -> Nothing
  _ -> Nothing

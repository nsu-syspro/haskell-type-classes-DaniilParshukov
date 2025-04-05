{-# OPTIONS_GHC -Wall #-}

module Task1 where

data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show

evalIExpr :: IExpr -> Integer
evalIExpr (Lit n) = n
evalIExpr (Add e1 e2) = evalIExpr e1 + evalIExpr e2
evalIExpr (Mul e1 e2) = evalIExpr e1 * evalIExpr e2

class Parse a where
  parse :: String -> Maybe a

parseRPN :: [String] -> [IExpr] -> Maybe IExpr
parseRPN [] [expr] = Just expr
parseRPN [] _ = Nothing
parseRPN (token:tokens) stack = case token of
  "+" -> case stack of
           e2:e1:rest -> parseRPN tokens (Add e1 e2 : rest)
           _ -> Nothing
  "*" -> case stack of
           e2:e1:rest -> parseRPN tokens (Mul e1 e2 : rest)
           _ -> Nothing
  num -> case reads num of
          [(n, "")] -> parseRPN tokens (Lit n : stack)
          _ -> Nothing

instance Parse IExpr where
  parse s = parseRPN (words s) []

instance Parse Integer where
  parse s = case reads s of
             [(n, "")] -> Just n
             _         -> Nothing

instance Parse Bool where
  parse "true"  = Just True
  parse "false" = Just False
  parse "1"     = Just True
  parse "0"     = Just False
  parse _       = Nothing

evaluateIExpr :: String -> Maybe Integer
evaluateIExpr s = case parse s of
                  Just expr -> Just (evalIExpr expr)
                  Nothing -> Nothing
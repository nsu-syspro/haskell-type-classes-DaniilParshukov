{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task2 where

import Task1 (Parse, parse)

data Expr a op =
    Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving Show

data IntOp = Add | Mul | Sub
  deriving Show

instance (Parse a, Parse op) => Parse (Expr a op) where
  parse s = parseRPN (words s) []
    where
      parseRPN :: [String] -> [Expr a op] -> Maybe (Expr a op)
      parseRPN [] [expr] = Just expr
      parseRPN [] _ = Nothing
      parseRPN (token:tokens) stack = case parseOp token of
        Just op -> handleOp op tokens stack
        Nothing -> handleNonOp token tokens stack
      
      parseOp :: String -> Maybe op
      parseOp = parse
      
      handleOp :: op -> [String] -> [Expr a op] -> Maybe (Expr a op)
      handleOp op tokens (e2:e1:rest) = parseRPN tokens (BinOp op e1 e2 : rest)
      handleOp _ _ _ = Nothing
      
      handleNonOp :: String -> [String] -> [Expr a op] -> Maybe (Expr a op)
      handleNonOp token tokens stack
        | isVar token = parseRPN tokens (Var token : stack)
        | otherwise = case parse token of
                      Just val -> parseRPN tokens (Lit val : stack)
                      Nothing -> Nothing
      
      isVar :: String -> Bool
      isVar var = all (\c -> c == '_' || c `elem` ['a'..'z'] || c `elem` ['A'..'Z']) var

class Eval a op where
  evalBinOp :: op -> a -> a -> a

instance Eval Integer IntOp where
  evalBinOp Add = (+)
  evalBinOp Mul = (*)
  evalBinOp Sub = (-)

evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr _ (Lit val) = Just val
evalExpr env (Var name) = lookup name env
evalExpr env (BinOp op e1 e2) = do
  v1 <- evalExpr env e1
  v2 <- evalExpr env e2
  Just $ evalBinOp op v1 v2

type Reify a op = Expr a op -> Expr a op

reifyInteger :: Reify Integer IntOp
reifyInteger = id

instance Parse IntOp where
  parse "+" = Just Add
  parse "-" = Just Sub
  parse "*" = Just Mul
  parse _ = Nothing

evaluate :: (Eval a op, Parse a, Parse op) => Reify a op -> [(String, a)] -> String -> Maybe a
evaluate reify m s = case parse s of
  Just e -> evalExpr m (reify e)
  Nothing -> Nothing

evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger = evaluate reifyInteger
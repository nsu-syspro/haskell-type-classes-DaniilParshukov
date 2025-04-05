{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Task3 where

import Data.List (nub)
import Task1 (Parse, parse)
import Task2 (Expr(..), Eval(..), evalExpr)


data BoolOp = And | Or | Xor
  deriving Show

instance Parse BoolOp where
  parse "and" = Just And
  parse "or"  = Just Or
  parse "xor" = Just Xor
  parse _     = Nothing

instance Eval Bool BoolOp where
  evalBinOp And = (&&)
  evalBinOp Or  = (||)
  evalBinOp Xor = \x y -> x /= y

solveSAT :: String -> Maybe Bool
solveSAT input = do
  expr <- parse input :: Maybe (Expr Bool BoolOp)
  let vars = nub (getVars expr)
  if null vars
    then evalExpr [] expr
    else or <$> sequence [evalExpr (zip vars vals) expr | vals <- allCombinations (length vars)]
  
getVars :: Expr a op -> [String]
getVars (Var name) = [name]
getVars (Lit _) = []
getVars (BinOp _ e1 e2) = getVars e1 ++ getVars e2

allCombinations :: Int -> [[Bool]]
allCombinations n = sequence (replicate n [False, True])
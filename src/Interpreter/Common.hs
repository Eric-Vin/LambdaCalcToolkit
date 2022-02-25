module Interpreter.Common where

import Data.Map (Map)

-- Lambda Calc Datatypes

type LambdaVar = String

data LambdaExpr = Function [LambdaVar] [LambdaExpr]
                | Var LambdaVar
                deriving (Show, Eq)

type LambdaVarMap = Map LambdaVar LambdaVar

-- Utility Functions
boundVars :: LambdaExpr -> [LambdaVar]
boundVars (Var v) = []
boundVars (Function vars lexprs) =vars ++ (foldl foldVars [] lexprs)
    where
    foldVars vs lexpr = vs ++ (boundVars lexpr)

-- Encoding Helper Functions
encodeInt :: Int -> LambdaExpr
encodeInt i   = Function [func_var, input_var] (lexpr i)
    where
        func_var    = "f"
        input_var   = "x"

        lexpr j     =   if j == 0
                        then [(Var input_var)]
                        else (Var func_var):(lexpr (j-1))

encodeBool :: Bool -> LambdaExpr
encodeBool True     = Function ["x", "y"] [(Var "x")]
encodeBool False    = Function ["x", "y"] [(Var "y")]

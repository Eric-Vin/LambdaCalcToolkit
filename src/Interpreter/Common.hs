module Interpreter.Common where

-- Lambda Calc Datatypes

type LambdaVar = String

data LambdaExpr = Function [LambdaVar] [LambdaExpr]
                | Var LambdaVar

-- Encoding Helper Functions
encodeInt :: Int -> LambdaExpr
encodeInt i   = undefined

encodeBool :: Bool -> LambdaExpr
encodeBool b  = undefined

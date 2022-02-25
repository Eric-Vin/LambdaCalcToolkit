module Interpreter.Common where

-- Lambda Calc Datatypes

type LambdaVar = String

data LambdaExpr = Function [LambdaVar] [LambdaExpr]
                | Var LambdaVar

-- Encoding Helper Functions
encodeInt :: Int -> LambdaExpr
encodeInt i   = Function [func_var, input_var] lexpr 
    where
        func_var    = Var "f"
        input_var   = Var "x"
        lexpr i     =   if i == 0
                        then input_var
                        else func_var:(lexpr (i-1))

encodeBool :: Bool -> LambdaExpr
encodeBool True     = Function [(Var "x"), (Var "y")] [(Var "x")]
encodeBool False    = Function [(Var "x"), (Var "y")] [(Var "y")]

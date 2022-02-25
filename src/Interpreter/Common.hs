module Interpreter.Common where

import Data.Map (Map)
import Data.List (intercalate)

-- Lambda Calc Datatypes

type LambdaVar = String

data LambdaExpr = Function LambdaVar [LambdaExpr]
                | Application [LambdaExpr] [LambdaExpr]
                | Var LambdaVar
                deriving (Eq)

instance Show LambdaExpr where
    show (Function var lexprs) = "(\\" ++ var ++ " -> " ++ (intercalate " " $ map show lexprs) ++ ")"
    show (Application lexprs1 lexprs2) = "(\\" ++ (intercalate " " $ map show lexprs1) ++ " -> " ++ (intercalate " " $ map show lexprs2) ++ ")"
    show (Var var)             = var

type LambdaVarMap = Map LambdaVar LambdaVar
type LambdaExprMap = Map LambdaVar LambdaExpr

-- Utility Functions
boundVars :: LambdaExpr -> [LambdaVar]
boundVars (Var v) = []
boundVars (Function var lexprs) =var:(foldl foldVars [] lexprs)
    where
    foldVars vs lexpr = vs ++ (boundVars lexpr)

-- Encoding Helper Functions
encodeNat :: Int -> LambdaExpr
encodeNat i   = Function func_var [Function input_var (lexpr i)]
    where
        func_var    = "f"
        input_var   = "x"

        lexpr j     =   if j == 0
                        then [(Var input_var)]
                        else (Var func_var):(lexpr (j-1))

encodeBool :: Bool -> LambdaExpr
encodeBool True     = Function "x" [Function "y" [(Var "x")]]
encodeBool False    = Function "x" [Function "y" [(Var "y")]]

isLambdaTrue :: [LambdaExpr] -> Bool
isLambdaTrue [(Function var1 [(Function var2 [Var var3])])] = (var1 /= var2) && (var1 == var3)
isLambdaTrue _ = False

isLambdaFalse :: [LambdaExpr] -> Bool
isLambdaFalse [(Function var1 [(Function var2 [Var var3])])] = (var1 /= var2) && (var2 == var3)
isLambdaFalse _ = False

isLambdaNum :: [LambdaExpr] -> Bool
isLambdaNum lexprs = False
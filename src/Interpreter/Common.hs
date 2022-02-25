module Interpreter.Common where

import Data.Map (Map)
import Data.List (intercalate)

-- Lambda Calc Datatypes

type LambdaVar = String

data LambdaExpr = Function LambdaVar LambdaExpr
                | Application LambdaExpr LambdaExpr
                | Var LambdaVar
                deriving (Show, Eq)

newtype PrettyLambdaExpr = Pretty LambdaExpr

instance Show PrettyLambdaExpr where
    show (Pretty (Function var lexpr))        = "(\\" ++ var ++ " -> " ++ (show (Pretty lexpr)) ++ ")"
    show (Pretty (Application lexpr1 lexpr2)) = "(" ++ (show (Pretty lexpr1)) ++ " " ++ (show (Pretty lexpr2)) ++ ")"
    show (Pretty (Var var))                   = var

type LambdaVarMap = Map LambdaVar LambdaVar

-- Utility Functions
boundVars :: LambdaExpr -> [LambdaVar]
boundVars (Var v)                       = []
boundVars (Function var lexpr)          = var:(boundVars lexpr)
boundVars (Application lexpr1 lexpr2)   = (boundVars lexpr1) ++ (boundVars lexpr2)

-- Encoding Helper Functions
encodeNat :: Int -> LambdaExpr
encodeNat i   = Function func_var (Function input_var (lexpr i))
    where
        func_var    = "f"
        input_var   = "x"

        lexpr j     =   if j == 0
                        then (Var input_var)
                        else Application (Var func_var) (lexpr (j-1))

encodeBool :: Bool -> LambdaExpr
encodeBool True     = Function "x" (Function "y" ((Var "x")))
encodeBool False    = Function "x" (Function "y" ((Var "y")))

isLambdaTrue :: LambdaExpr -> Bool
isLambdaTrue (Function var1 (Function var2 (Var var3))) = (var1 /= var2) && (var1 == var3)
isLambdaTrue _ = False

isLambdaFalse :: LambdaExpr -> Bool
isLambdaFalse (Function var1 (Function var2 (Var var3))) = (var1 /= var2) && (var2 == var3)
isLambdaFalse _ = False

isLambdaNum :: LambdaExpr -> Bool
isLambdaNum (Function var1 (Function var2 lexpr)) = confirm_internal lexpr
    where
        confirm_internal (Application (Var x) (Var y)) = (x == var1) && (y == var2)
        confirm_internal (Application (Var x) y) = (x == var1) && (confirm_internal y)
        confirm_internal _ = False

isLambdaNum _ = False

convertLambdaNum :: LambdaExpr -> Int
convertLambdaNum (Function var1 (Function var2 lexpr)) = count_internal lexpr
    where
        count_internal (Application (Var x) (Var y)) = 1
        count_internal (Application (Var x) y) = 1 + (count_internal y)

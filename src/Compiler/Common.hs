module Compiler.Common where

import Data.Map (Map)
import Data.List (intercalate, sortOn)

-- Interpreter datatypes
newtype State = State [(String, Int)]

addState :: State -> String -> Int -> State
addState state name val  = State $ update_state state_list name val
    where
        State state_list        = state

        update_state :: [(String, Int)] -> String -> Int -> [(String, Int)]
        update_state [] n v     = [(n,v)]
        update_state (x:xs) n v = if (fst x) == n
                                then
                                    (n, v):xs
                                else
                                    x:(update_state xs n v)

lookupState :: State -> String -> Int
lookupState state name  = find_val state_list name
    where
        State state_list    = state

        find_val :: [(String, Int)] -> String -> Int
        find_val [] n       = 0
        find_val (x:xs) n   = if (fst x) == n
                            then
                                snd x
                            else
                                find_val xs n

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

encodeNat :: Int -> LambdaExpr
encodeNat i   = Function func_var (Function input_var (lexpr i (Var input_var)))
    where
        func_var    = "f"
        input_var   = "x"

        lexpr j c_expr  =   if j == 0
                            then c_expr
                            else lexpr (j-1) (Application (Var func_var) c_expr)

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
        confirm_internal (Application (Var x) y) = if (x == var1)
                                                   then (confirm_internal y)
                                                   else False
        confirm_internal _ = False

isLambdaNum _ = False


instance Show State where
    show (State xs) = '{':(state_list ++ "}")
        where
            state_list = intercalate ", " $ map show_state $ sortOn fst xs
            show_state (name, val) = name ++ " → " ++ (show val)

-- The AST for While
data Program = Program Input Command Output

type Input = Maybe [String]
type Output = Maybe String

data Command
    = Semi Command Command
    | Skip
    | Assign String AExpr
    | If BExpr Command Command 
    | While BExpr Command
    deriving Show

data BExpr
    = Boolean Bool
    | Eq AExpr AExpr 
    | Less AExpr AExpr
    | Not BExpr
    | And BExpr BExpr
    | Or BExpr BExpr
    deriving Show

data AExpr 
    = Number Int
    | Variable String
    | Sub AExpr AExpr      
    | Add AExpr AExpr
    | Mult AExpr AExpr
    | Neg AExpr
    deriving Show

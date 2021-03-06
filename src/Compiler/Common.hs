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

instance Show State where
    show (State xs) = '{':(state_list ++ "}")
        where
            state_list = intercalate ", " $ map show_state $ sortOn fst xs
            show_state (name, val) = name ++ " → " ++ (show val)

-- The AST for While
data Program = Program Input Command

type Input = [String]

data Command
    = Semi Command Command
    | Skip
    | Assign String AExpr
    | BAssign String BExpr
    | If BExpr Command Command 
    | While BExpr Command
    | Dummy String
    deriving Show

data BExpr
    = Boolean Bool
    | BVariable String
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


getAssignedVars :: Command -> [String]
getAssignedVars (Semi c1 c2)  = (getAssignedVars c1) ++ (getAssignedVars c2)
getAssignedVars Skip          = []
getAssignedVars (Assign s a)  = [s]
getAssignedVars (BAssign s a) = [s]
getAssignedVars (If b c1 c2)  = (getAssignedVars c1) ++ (getAssignedVars c2)
getAssignedVars (While b c)   = (getAssignedVars c)
getAssignedVars (Dummy c)     = []


getAllVars :: Command -> [String]
getAllVars (Semi c1 c2)  = (getAssignedVars c1) ++ (getAssignedVars c2)
getAllVars Skip          = []
getAllVars (Assign s a)  = [s] ++ (getAExprVars a)
getAllVars (BAssign s b) = [s] ++ (getBExprVars b)
getAllVars (If b c1 c2)  = (getBExprVars b) ++ (getAssignedVars c1) ++ (getAssignedVars c2)
getAllVars (While b c)   = (getBExprVars b) ++ (getAssignedVars c)
getAllVars (Dummy c)     = []

getAExprVars :: AExpr -> [String]
getAExprVars (Number _)     = []
getAExprVars (Variable s)   = [s]
getAExprVars (Sub a1 a2)    = (getAExprVars a1) ++ (getAExprVars a2)
getAExprVars (Add a1 a2)    = (getAExprVars a1) ++ (getAExprVars a2)
getAExprVars (Mult a1 a2)   = (getAExprVars a1) ++ (getAExprVars a2)
getAExprVars (Neg a)        = (getAExprVars a)

getBExprVars :: BExpr -> [String]
getBExprVars (Boolean _)    = []
getBExprVars (BVariable s)  = [s]
getBExprVars (Eq a1 a2)     = (getAExprVars a1) ++ (getAExprVars a2)
getBExprVars (Less a1 a2)   = (getAExprVars a1) ++ (getAExprVars a2)
getBExprVars (And b1 b2)    = (getBExprVars b1) ++ (getBExprVars b2)
getBExprVars (Or b1 b2)     = (getBExprVars b1) ++ (getBExprVars b2)
getBExprVars (Not b)        = (getBExprVars b)
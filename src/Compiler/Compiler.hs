module Compiler.Compiler (runCompiler) where

import Compiler.Common
import Compiler.Parser (parser)
import Compiler.Lexer (lexer)
import System.IO
import Debug.Trace

runCompiler :: String -> String -> IO ()
runCompiler inFile outFile = do
    input <- readFile inFile
    print $ (evalCommand (State []) . parser . lexer) input


-- Interpreter for the AST
evalCommand :: State -> Command -> State 
evalCommand s (Semi c1 c2) = evalCommand s' c2
    where
        s' = evalCommand s c1

evalCommand s Skip = s

evalCommand s (Assign name a) = s'
    where
        val = evalAExpr s a
        s' = addState s name val

evalCommand s (If b c1 c2) = s'
    where
        condition = evalBExpr s b

        s' =    if condition
                then
                    evalCommand s c1
                else
                    evalCommand s c2

evalCommand s (While b c) = fin_state
    where
        condition = evalBExpr s b

        mid_state = evalCommand s c

        fin_state = if condition
                    then
                        evalCommand mid_state (While b c)
                    else
                        s

evalBExpr :: State -> BExpr -> Bool
evalBExpr _ (Boolean b) = b

evalBExpr s (Eq a1 a2)  = val1 == val2
    where
        val1 = evalAExpr s a1
        val2 = evalAExpr s a2

evalBExpr s (Less a1 a2) = val1 < val2
    where
        val1 = evalAExpr s a1
        val2 = evalAExpr s a2

evalBExpr s (Not b) = not val
    where
        val = evalBExpr s b

evalBExpr s (And b1 b2) = val1 && val2
    where
        val1 = evalBExpr s b1
        val2 = evalBExpr s b2

evalBExpr s (Or b1 b2) = val1 || val2
    where
        val1 = evalBExpr s b1
        val2 = evalBExpr s b2

evalAExpr :: State -> AExpr -> Int
evalAExpr _ (Number num) = num

evalAExpr s (Var name) = val
    where
        val = lookupState s name

evalAExpr s (Sub a1 a2) = val1 - val2
    where
        val1 = evalAExpr s a1
        val2 = evalAExpr s a2

evalAExpr s (Add a1 a2) = val1 + val2
    where
        val1 = evalAExpr s a1
        val2 = evalAExpr s a2

evalAExpr s (Mult a1 a2) = val1 * val2
    where
        val1 = evalAExpr s a1
        val2 = evalAExpr s a2

evalAExpr s (Neg a) = -1 * val
    where
        val = evalAExpr s a

evalAExpr s (Ternary b a1 a2) = val 
    where
        condition = evalBExpr s b

        val =    if condition
                then
                    evalAExpr s a1
                else
                    evalAExpr s a2
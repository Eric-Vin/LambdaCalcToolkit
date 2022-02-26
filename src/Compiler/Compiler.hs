module Compiler.Compiler (runCompiler) where

import Compiler.Common
import Compiler.Parser (parser)
import Compiler.Lexer (lexer)
import System.IO
import Debug.Trace

runCompiler :: String -> String -> IO ()
runCompiler inFile outFile = do
    inputHandle <- openFile inFile ReadMode 
    hSetEncoding inputHandle utf8
    input <- hGetContents inputHandle

    putStrLn $ (showString ((evalCommand (State []) . parser . lexer) input) [])


-- Interpreter for the AST
evalCommand :: State -> Command -> String 
evalCommand s (Semi c1 c2) = " "
    where
        s' = evalCommand s c1

evalCommand s Skip = " "

evalCommand s (Assign name a) = val
    where 
        val = evalAExpr s a



evalCommand s (If b c1 c2) = "((((\\p -> (\\a ->(\\b -> ((p a) b)))) " ++ bool ++ ")" ++ com1 ++ ")" ++ com2 ++ ")"
    where 
        bool = evalBExpr s b
        com1 = evalCommand s c1
        com2 = evalCommand s c2
    

evalCommand s (While b c) = " "

evalBExpr :: State -> BExpr -> String
evalBExpr _ (Boolean b) = val
    where
        val = show (Pretty (Compiler.Common.encodeBool b))

evalBExpr s (Eq a1 a2)  = "(((\\x -> (\\y -> ((\\n-> ((n (\\x -> (\\x -> (\\y -> y)))) (\\x -> (\\y -> x)))) (((\\m ->(\\n->((n (\\k ->(\\f->(\\x->(f ((k f) x)))))) m))) (((\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) x) y)) (((\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) y) x))))) " ++ val1 ++ ") " ++ val2 ++ ")"
    where 
        val1 = evalAExpr s a1
        val2 = evalAExpr s a2
evalBExpr s (Less a1 a2) = "(((\\x -> (\\y -> (((\\x -> (\\y -> ((x y) x))) (((\\x -> (\\y -> ((\\n-> ((n (\\x -> (\\x -> (\\y -> y)))) (\\x -> (\\y -> x)))) (((\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) x) y)))) x) y)) ((\\a -> (\\x ->(\\y -> ((a y) x)))) (((\\x -> (\\y -> ((\\n-> ((n (\\x -> (\\x -> (\\y -> y)))) (\\x -> (\\y -> x)))) (((\\m ->(\\n->((n (\\k ->(\\f->(\\x->(f ((k f) x)))))) m))) (((\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) x) y)) (((\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) y) x))))) x) y))))) " ++ val1 ++ ") " ++ val2 ++ ")"
    where 
        val1 = evalAExpr s a1
        val2 = evalAExpr s a2

evalBExpr s (Not b) = "((\a -> (\\x -> (\\y -> ((a y) x)))) " ++ val ++ ")"
    where 
        val = evalBExpr s b

evalBExpr s (And b1 b2) = "(((\\x -> (\\y -> ((x y) x))) " ++ val1 ++ ") " ++ val2 ++ ")"
    where 
        val1 = evalBExpr s b1 
        val2 = evalBExpr s b2

evalBExpr s (Or b1 b2) = "(((\\x -> (\\y -> ((x x) y))) " ++ val1 ++ ") " ++ val2 ++ ")"
    where 
        val1 = evalBExpr s b1 
        val2 = evalBExpr s b2

evalAExpr :: State -> AExpr -> String
evalAExpr _ (Number num) = val
    where
        val = show (Pretty (Compiler.Common.encodeNat num))

evalAExpr s (Variable name) = show (Pretty (Compiler.Common.encodeNat 0))

evalAExpr s (Sub a1 a2) = "(((\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) " ++ val1 ++ ") " ++ val2 ++ ")"
    where
        val1 = evalAExpr s a1 
        val2 = evalAExpr s a2

evalAExpr s (Add a1 a2) = "(((\\m ->(\\n->((n (\\k ->(\\f->(\\x->(f ((k f) x)))))) m))) " ++ val1 ++ ") " ++ val2 ++ ")"
    where
        val1 = evalAExpr s a1 
        val2 = evalAExpr s a2


evalAExpr s (Mult a1 a2) = "(((\\m->(\\n->(\\f->(m (n f))))) " ++ val1 ++ ") " ++ val2 ++ ")"
    where
        val1 = evalAExpr s a1 
        val2 = evalAExpr s a2

evalAExpr s (Neg a) = show (Pretty (Compiler.Common.encodeNat 0))


module Compiler.Compiler (runCompiler) where

import Compiler.Common
import Data.List
import Interpreter.Common
import Compiler.Parser (parser)
import Compiler.Lexer (lexer)
import System.IO
import Debug.Trace

runCompiler :: String -> String -> IO ()
runCompiler inFile outFile = do
    inputHandle <- openFile inFile ReadMode 
    hSetEncoding inputHandle utf8
    input <- hGetContents inputHandle
    writeFile outFile $ (showString ((evalProgram . parser . lexer) input) [])

evalProgram :: Program -> String
evalProgram (Program input cmd) = intializeVars input cmd

intializeVars :: [String] -> Command -> String
intializeVars input cmd =  start_wrap ++ program ++ end_wrap
    where
        zero = show (Pretty (Interpreter.Common.encodeNat 0))
        vars = nub (Compiler.Common.getAllVars cmd) \\ input
        lambda_vars = map (\x -> x ++ "-> ")  (map (\x -> "((\\" ++ x) vars)
        start_wrap = (concat lambda_vars)
        end_wrap = (concat (replicate (length vars) (") " ++ zero ++ ")") ))
        program = initializeInput (reverse input) cmd

initializeInput :: [String] -> Command -> String
initializeInput [] cmd = evalCommand (State []) cmd
initializeInput (x:xs) cmd = "(\\" ++ x ++ " -> " ++ program ++ ")"
    where
        program = initializeInput xs cmd

-- Interpreter for the AST
evalCommand :: State -> Command -> String 
evalCommand s (Semi (Assign name a) c2) = "((\\" ++ name ++ "-> " ++ com ++ ") " ++ val ++ ")"
    where
        com = evalCommand s c2
        val = evalAExpr s a

evalCommand s (Semi (BAssign name b) c2) = "((\\" ++ name ++ "-> " ++ com ++ ") " ++ val ++ ")"
    where
        com = evalCommand s c2
        val = evalBExpr s b


evalCommand s (Semi (If b c1 c2) c3) = "(((" ++ ifthen ++ bool ++ ")" ++ com1 ++ ")" ++ com2 ++ ")"
    where
        ifthen = "(\\p -> (\\a ->(\\b -> ((p a) b)))) "
        com1 = evalCommand s (Semi c1 c3)
        com2 = evalCommand s (Semi c2 c3)
        bool = evalBExpr s b

evalCommand s (Semi (While b c1) c2) = final
    where
        final = (concat   (replicate (length vars) "(" )) ++ "(" ++ y_comb ++ while_rec ++ ")"++ (intercalate  ") " vars) ++ ")"
        y_comb = "(\\x -> ((\\y-> (x (y y))) (\\y-> (x (y y)))))"
        while_rec = "(\\w -> " ++ (concat lambda_vars) ++ outer_body ++ (concat (replicate (length vars) ")" )) ++ ")"
        outer_body = "((" ++ bool ++ com ++ ")" ++ after ++ ")"
        bool = evalBExpr s b
        com = evalCommand s (Semi c1 (Dummy dummy_vars))
        after = evalCommand s c2
        lambda_vars = map (\x -> x ++ "-> ")  (map (\x -> "(\\" ++ x) vars)
        dummy_vars = (concat   (replicate (length vars) "(" )) ++ "w " ++ (intercalate  ") " vars) ++ ")"
        vars = Compiler.Common.getAssignedVars c1

evalCommand s (Semi (Semi c1 c2) c3) = com
    where
        com = evalCommand s (Semi c1 (Semi c2 c3))


evalCommand s (Dummy op ) = op

evalCommand s (Semi Skip c) = com
    where
       com = evalCommand s c

evalCommand s Skip = " "

evalCommand s (Assign name a) = val
    where 
        val = evalAExpr s a

evalCommand s (BAssign name a) = val
    where 
        val = evalBExpr s a


evalCommand s (If b c1 c2) = "(((" ++ ifthen ++ bool ++ ")" ++ com1 ++ ")" ++ com2 ++ ")"
    where 
        ifthen = "(\\p -> (\\a ->(\\b -> ((p a) b)))) "
        bool = evalBExpr s b
        com1 = evalCommand s c1
        com2 = evalCommand s c2
    

evalCommand s (While b c) = final
    where
        final = (concat   (replicate (length vars) "(" )) ++ "(" ++ y_comb ++ while_rec ++ ")"++ (intercalate  ") " vars) ++ ")"
        y_comb = "(\\x -> ((\\y-> (x (y y))) (\\y-> (x (y y)))))"
        while_rec = "(\\w -> " ++ (concat lambda_vars) ++ outer_body ++ (concat (replicate (length vars) ")" )) ++ ")"
        outer_body = "((" ++ bool ++ com ++ ")" ++ "" ++ ")"
        bool = evalBExpr s b
        com = evalCommand s (Semi c (Dummy dummy_vars))
        lambda_vars = map (\x -> x ++ "-> ")  (map (\x -> "(\\" ++ x) vars)
        dummy_vars = (concat   (replicate (length vars) "(" )) ++ "w " ++ (intercalate  ") " vars) ++ ")"
        vars = Compiler.Common.getAssignedVars c

evalBExpr :: State -> BExpr -> String
evalBExpr _ (Boolean b) = val
    where
        val = show (Pretty (Interpreter.Common.encodeBool b))

evalBExpr s (BVariable name) = name

evalBExpr s (Eq a1 a2)  = "((" ++ equal ++ val1 ++ ") " ++ val2 ++ ")"
    where 
        equal = "(\\x -> (\\y -> ((\\n-> ((n (\\x -> (\\x -> (\\y -> y)))) (\\x -> (\\y -> x)))) (((\\m ->(\\n->((n (\\k ->(\\f->(\\x->(f ((k f) x)))))) m))) (((\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) x) y)) (((\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) y) x))))) "
        val1 = evalAExpr s a1
        val2 = evalAExpr s a2
evalBExpr s (Less a1 a2) = "((" ++ le ++ val1 ++ ") " ++ val2 ++ ")"
    where 
        le = "(\\x -> (\\y -> (((\\x -> (\\y -> ((x y) x))) (((\\x -> (\\y -> ((\\n-> ((n (\\x -> (\\x -> (\\y -> y)))) (\\x -> (\\y -> x)))) (((\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) x) y)))) x) y)) ((\\a -> (\\x ->(\\y -> ((a y) x)))) (((\\x -> (\\y -> ((\\n-> ((n (\\x -> (\\x -> (\\y -> y)))) (\\x -> (\\y -> x)))) (((\\m ->(\\n->((n (\\k ->(\\f->(\\x->(f ((k f) x)))))) m))) (((\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) x) y)) (((\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) y) x))))) x) y))))) "
        val1 = evalAExpr s a1
        val2 = evalAExpr s a2

evalBExpr s (Not b) = "(" ++ neg ++ val ++ ")"
    where 
        val = evalBExpr s b
        neg = "(\\a -> (\\x -> (\\y -> ((a y) x)))) "

evalBExpr s (And b1 b2) = "((" ++ land ++ val1 ++ ") " ++ val2 ++ ")"
    where 
        land = "(\\x -> (\\y -> ((x y) x))) "
        val1 = evalBExpr s b1 
        val2 = evalBExpr s b2

evalBExpr s (Or b1 b2) = "((" ++ lor ++ val1 ++ ") " ++ val2 ++ ")"
    where 
        lor = "(\\x -> (\\y -> ((x x) y))) "
        val1 = evalBExpr s b1 
        val2 = evalBExpr s b2

evalAExpr :: State -> AExpr -> String
evalAExpr _ (Number num) = val
    where
        val = show (Pretty (Interpreter.Common.encodeNat num))

evalAExpr s (Variable name) = name

evalAExpr s (Sub a1 a2) = "((" ++ sub ++ val1 ++ ") " ++ val2 ++ ")"
    where
        sub  = "(\\a -> (\\b -> ((b (\\n -> (\\f -> (\\x -> (((n (\\g -> (\\h -> (h (g f))))) (\\u->x)) (\\u->u)))))) a))) "
        val1 = evalAExpr s a1 
        val2 = evalAExpr s a2

evalAExpr s (Add a1 a2) = "((" ++ plus ++ val1 ++ ") " ++ val2 ++ ")"
    where
        plus = "(\\m ->(\\n->((n (\\k ->(\\f->(\\x->(f ((k f) x)))))) m))) "
        val1 = evalAExpr s a1 
        val2 = evalAExpr s a2


evalAExpr s (Mult a1 a2) = "((" ++ times ++ val1 ++ ") " ++ val2 ++ ")"
    where
        times = "(\\m->(\\n->(\\f->(m (n f))))) "
        val1 = evalAExpr s a1 
        val2 = evalAExpr s a2

evalAExpr s (Neg a) = show (Pretty (Interpreter.Common.encodeNat 0))


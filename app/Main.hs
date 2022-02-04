module Main where

import System.Environment

import Interpreter.Interpreter(runInterpreter)
import Compiler.Compiler(runCompiler)

main :: IO ()
main = do
        args    <- getArgs

        let mode = head args
        let mode_args = tail args
        
        case head args of
            "interpret"     -> runInterpreter (head mode_args) (tail mode_args) >>= putStrLn
            "compile"       -> runCompiler (head mode_args) (head $ tail mode_args)




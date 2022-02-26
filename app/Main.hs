module Main where

import System.Environment
import System.IO
import qualified System.IO.Utf8 as Utf8

import Interpreter.Interpreter(runInterpreter)
import Compiler.Compiler(runCompiler)

main :: IO ()
main = do
        hSetEncoding stdin utf8
        hSetEncoding stdout utf8
        args    <- getArgs

        let mode = head args
        let mode_args = tail args
        
        case head args of
            "interpret"     -> runInterpreter (head mode_args) (drop 1 mode_args) >>= print
            "compile"       -> runCompiler (head mode_args) (head $ tail mode_args)




module Interpreter.Interpreter (runInterpreter) where

import Interpreter.Common

runInterpreter :: String -> [String] -> IO String
runInterpreter file_path params = undefined

parseInputs :: Int -> [String] -> [LambdaExpr]
parseInputs _ []    = []

parseInputs i x:xs  = (encode x):(parseInputs i' xs)
    where
        encode var  | var == "True"     = encodeBool True
                    | var == "False"    = encodeBool False
                    | all isDigit var   = encodeInt var

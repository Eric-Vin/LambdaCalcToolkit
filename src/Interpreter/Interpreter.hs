module Interpreter.Interpreter (runInterpreter) where

import Data.Char
import Data.List (intercalate)
import Data.Map (empty, (!), insert, member, fromList)

import Interpreter.Common
import Interpreter.Lexer
import Interpreter.Parser

runInterpreter :: String -> [String] -> IO String
runInterpreter file_path params = do
    raw_file      <- readFile file_path
    let parsed_lexpr  = (parser . lexer) raw_file
    let enc_params    = encodeInputs params
    let out_lexpts    = interpret (applyInputs parsed_lexpr enc_params)
    return $ decodeOutput out_lexpts

encodeInputs :: [String] -> [LambdaExpr]
encodeInputs []    = []
encodeInputs (x:xs)  = (encode x):(encodeInputs xs)
    where
        encode var  | var == "True"     = encodeBool True
                    | var == "False"    = encodeBool False
                    | all isDigit var   = encodeNat $ read var
                    | otherwise         = error ("Unrecognized input '" ++ var ++ "'")

applyInputs :: LambdaExpr -> [LambdaExpr] -> LambdaExpr
applyInputs f []    = f
applyInputs f x:xs  = Application (applyInputs xs) x

decodeOutput :: LambdaExpr -> String
decodeOutput lexprs
    | isLambdaTrue lexprs   = "True"
    | isLambdaFalse lexprs  = "False/0"
    | isLambdaNum lexprs    = show $ convertLambdaNum lexprs
    | otherwise             = show lexprs

interpret :: LambdaExpr -> LambdaExpr
interpret app@(Application lexpr1 lexpr2) = 
    case lexpr1 of
        (Function var fun_lexpr)    -> applied_func
        _                           -> app
    where
        bound_vars = boundVars lexpr1
        a_lexpr2 = alphaConversion bound_vars lexpr2

        applied_func = betaReduction lexpr1 a_lexpr2

interpret lexpr = lexpr

-- For each LamdaExpr, replaces all instances of variables in [LambdaVar]
-- with a different variable
alphaConversion :: [LambdaVar] -> LambdaExpr -> LambdaExpr
alphaConversion vars lexpr = alpha_output
    where
        (alpha_output,_,_,_) = alphaMapLexpr vars empty 0 lexpr

        alphaMapLexpr :: [LambdaVar] -> LambdaVarMap -> Int -> LambdaExpr -> (LambdaExpr, [LambdaVar], LambdaVarMap, Int)
        alphaMapLexpr vars map i (Var v)  = if v `elem` vars
                                          then  if member v map
                                                then ((Var (map ! v)), vars, map, i)
                                                else ((Var new_var_name), vars', map', i')
                                          else ((Var v), vars, map, i)
            where
                new_var_name = "a" ++ (show i)
                vars' = new_var_name:vars
                map' = insert v new_var_name map
                i' = i + 1

        alphaMapLexpr vars map i (Function param lexpr) = ((Function mapped_param mapped_lexprs), vars'', map'', i'')
            where
                (mapped_param, vars', map', i') = alphaMapLvar vars map i param
                (mapped_lexprs, vars'', map'', i'') = alphaMapLexpr vars' map' i' lexpr

        alphaMapLexpr vars map i (Application lexpr1 lexpr2) = ((Application mapped_lexpr2 mapped_lexpr2), vars'', map'', i'')
            where
                (mapped_lexpr2, vars', map', i') = alphaMapLexpr vars map i lexpr1
                (mapped_lexpr2, vars'', map'', i'') = alphaMapLexpr vars' map' i' lexpr2

        alphaMapLvar :: [LambdaVar] -> LambdaVarMap -> Int -> LambdaVar -> (LambdaVar, [LambdaVar], LambdaVarMap, Int)
        alphaMapLvar vars map i v   = if v `elem` vars
                                      then  if member v map
                                            then ((map ! v), vars, map, i)
                                            else (new_var_name, vars', map', i')
                                      else ( v, vars, map, i)
            where
                new_var_name = "a" ++ (show i)
                vars' = new_var_name:vars
                map' = insert v new_var_name map
                i' = i + 1

betaReduction :: LambdaExpr -> LambdaExpr -> LambdaExpr
betaReduction (Function var lexpr) param = map (betaMapLexpr var param) lexpr
    where
        betaMapLexpr :: LambdaVar -> LambdaExpr -> LambdaExpr -> LambdaExpr
        betaMapLexpr rep inp (Var v) = if v == rep
                                       then inp
                                       else (Var v)

        betaMapLexpr rep inp (Function vars lexprs) = (Function var mapped_lexprs)
            where
                mapped_lexprs = map (betaMapLexpr rep inp) lexprs

        betaMapLexpr rep inp (Application lexpr1 lexpr2) = (Application lexpr1' lexpr2')
            where
                lexpr1' = map (betaMapLexpr rep inp) lexpr1
                lexpr2' = map (betaMapLexpr rep inp) lexpr2


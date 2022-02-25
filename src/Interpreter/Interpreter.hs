module Interpreter.Interpreter (runInterpreter) where

import Data.Char

import Data.Map (empty, (!), insert, member, fromList)

import Interpreter.Common
import Interpreter.Lexer
import Interpreter.Parser

runInterpreter :: String -> [String] -> IO String
runInterpreter file_path params = do
    raw_file      <- readFile file_path
    let parsed_lexprs = (parser . lexer) raw_file
    let enc_params    = encodeInputs params
    let out_lexpts    = interpret (parsed_lexprs ++ enc_params)
    return $ show out_lexpts

encodeInputs :: [String] -> [LambdaExpr]
encodeInputs []    = []
encodeInputs (x:xs)  = (encode x):(encodeInputs xs)
    where
        encode var  | var == "True"     = encodeBool True
                    | var == "False"    = encodeBool False
                    | all isDigit var   = encodeInt $ read var
                    | otherwise         = error ("Unrecognized input '" ++ var ++ "'")

interpret :: [LambdaExpr] -> [LambdaExpr]
interpret (func@(Function vs lexprs):xs) = 
    if null xs
    then [func]
    else interpret $ applied_func ++ rem_exprs
    where
        (in_expr, rem_exprs) = (head xs, tail xs)
        bound_vars = boundVars func
        fresh_in_exprs = alphaConversion bound_vars in_expr

        applied_func = betaReduction func in_expr

interpret ((Var v):xs) = (Var v):(interpret xs)

-- For each LamdaExpr, replaces all instances of variables in [LambdaVar]
-- with a different variable
alphaConversion :: [LambdaVar] -> LambdaExpr -> LambdaExpr
alphaConversion vars lexpr = alpha_output
    where
        (alpha_output,_,_,_) = alphaMapLexpr vars empty 0 lexpr

        alphaMapLexprs :: [LambdaVar] -> LambdaVarMap -> Int -> [LambdaExpr] -> ([LambdaExpr], [LambdaVar], LambdaVarMap, Int)
        alphaMapLexprs vars map i []        = ([], vars, map, i)
        alphaMapLexprs vars map i (x:xs)    = (mapped_lexpr:rem_lexprs, vars'', map'', i'')
            where
                (mapped_lexpr, vars', map', i') = alphaMapLexpr vars map i x
                (rem_lexprs, vars'', map'', i'') = alphaMapLexprs vars' map' i' xs

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

        alphaMapLexpr vars map i (Function param lexprs) = ((Function mapped_param mapped_lexprs), vars'', map'', i'')
            where
                (mapped_param, vars', map', i') = alphaMapLvar vars map i param
                (mapped_lexprs, vars'', map'', i'') = alphaMapLexprs vars' map' i' lexprs

        -- alphaMapLvars :: [LambdaVar] -> LambdaVarMap -> Int -> [LambdaVar] -> ([LambdaVar], [LambdaVar], LambdaVarMap, Int)
        -- alphaMapLvars vars map i []     = ([], vars, map, i)
        -- alphaMapLvars vars map i (x:xs) = (mapped_var:rem_vars, vars'', map'', i'')
        --     where
        --         (mapped_var, vars', map', i') = alphaMapLvar vars map i x
        --         (rem_vars, vars'', map'', i'') = alphaMapLvars vars' map' i' xs

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

betaReduction :: LambdaExpr -> LambdaExpr -> [LambdaExpr]
betaReduction (Function vars lexprs) params = mapped_body
    where
        mapped_body = undefined

        betaMapLexpr :: LambdaExprMap -> LambdaExpr -> LambdaExpr
        betaMapLexpr map expr =  undefined

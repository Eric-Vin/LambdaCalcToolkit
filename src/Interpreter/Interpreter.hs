module Interpreter.Interpreter (runInterpreter) where

import GHC.Stack

import Data.Char
import Data.List (intercalate)
import Data.Map (empty, (!), insert, member, fromList)

import Interpreter.Common
import Interpreter.Lexer
import Interpreter.Parser

import Debug.Trace

runInterpreter :: String -> [String] -> IO String
runInterpreter file_path params = do
    raw_file      <- readFile file_path

    let toks          = lexer raw_file
    let parsed_lexpr  = parser toks
    let enc_params    = encodeInputs $ reverse params
    let comp_expr     = applyInputs parsed_lexpr enc_params
    let out_lexpts    = interpretFixedPoint comp_expr

    let output        = if null toks
                        then []
                        else decodeOutput out_lexpts
    
    return output

encodeInputs :: [String] -> [LambdaExpr]
encodeInputs []     = []
encodeInputs (x:xs) = (encode x):(encodeInputs xs)
    where
        encode var  | var == "True"     = encodeBool True
                    | var == "False"    = encodeBool False
                    | all isDigit var   = encodeNat $ read var
                    | otherwise         = error ("Unrecognized input '" ++ var ++ "'")

applyInputs :: LambdaExpr -> [LambdaExpr] -> LambdaExpr
applyInputs f []        = f
applyInputs f (x:xs)    = Application (applyInputs f xs) x

decodeOutput :: LambdaExpr -> String
decodeOutput lexprs
    | isLambdaTrue lexprs   = "True"
    | isLambdaFalse lexprs  = "False/0"
    | isLambdaNum lexprs    = show $ convertLambdaNum lexprs
    | otherwise             = show (Pretty lexprs)

interpretFixedPoint :: LambdaExpr -> LambdaExpr
interpretFixedPoint lexpr = if progress
                            then interpretFixedPoint new_lexpr
                            else new_lexpr
    where
        used_vars = boundVars lexpr
        new_lexpr = interpret used_vars lexpr
        progress  = lexpr /= new_lexpr

interpret :: [LambdaVar] -> LambdaExpr -> LambdaExpr
interpret r_vars (Application lexpr1 lexpr2) = 
    case lexpr1 of
        (Function _ _)    -> applied_func
        _                 -> if new_lexpr_1 /= lexpr1
                             then Application new_lexpr_1 lexpr2
                             else Application lexpr1 new_lexpr_2
    where
        a_lexpr2 = alphaConversion r_vars lexpr2

        applied_func = betaReduction lexpr1 a_lexpr2

        new_lexpr_1 = interpret r_vars lexpr1
        new_lexpr_2 = interpret r_vars lexpr2

interpret r_vars (Function var lexpr) = Function var (interpret r_vars lexpr)

interpret r_vars (Var v) = (Var v)


-- For each LambdaExpr, replaces all instances of variables in [LambdaVar]
-- with a different variable
alphaConversion :: [LambdaVar] -> LambdaExpr -> LambdaExpr
alphaConversion reserved_vars lexpr = alpha_output
    where
        key_vars = boundVars lexpr

        alpha_map = createVarMap key_vars reserved_vars

        alpha_output = alphaMapLexpr alpha_map lexpr

        alphaMapLexpr :: LambdaVarMap -> LambdaExpr -> LambdaExpr
        alphaMapLexpr map (Var v)   =   if v `member` map
                                        then Var (map ! v)
                                        else Var v

        alphaMapLexpr map (Function param lexpr) = Function mapped_param mapped_lexprs
            where
                mapped_param = alphaMapLvar map param
                mapped_lexprs = alphaMapLexpr map lexpr

        alphaMapLexpr map (Application lexpr1 lexpr2) = Application mapped_lexpr1 mapped_lexpr2
            where
                mapped_lexpr1 = alphaMapLexpr map lexpr1
                mapped_lexpr2 = alphaMapLexpr map lexpr2

        alphaMapLvar :: LambdaVarMap -> LambdaVar -> LambdaVar
        alphaMapLvar map v  =   if v `member` map
                                then map ! v
                                else v

-- Creates a mapping from k_vars to a new set of variables, none of which are
-- in k_vars or r_vars
createVarMap :: [LambdaVar] -> [LambdaVar] -> LambdaVarMap
createVarMap k_vars r_vars = createVarMapHelper (k_vars ++ r_vars) 0 k_vars
    where
        createVarMapHelper :: [LambdaVar] -> Int -> [LambdaVar] -> LambdaVarMap
        createVarMapHelper banned_vars i []     = empty
        createVarMapHelper banned_vars i (v:vs) = insert v new_var (createVarMapHelper banned_vars i' vs)
            where
                (new_var, i') = getNewVar banned_vars i

        getNewVar :: [LambdaVar] -> Int -> (LambdaVar, Int)
        getNewVar banned_vars i =   if new_var `elem` banned_vars
                                    then getNewVar banned_vars (i+1)
                                    else (new_var, i+1)
            where
                new_var = "a" ++ (show i)

betaReduction :: LambdaExpr -> LambdaExpr -> LambdaExpr
betaReduction (Function var lexpr) param = betaMapLexpr var param lexpr
    where
        betaMapLexpr :: LambdaVar -> LambdaExpr -> LambdaExpr -> LambdaExpr
        betaMapLexpr rep inp (Var v) = if v == rep
                                       then inp
                                       else (Var v)

        betaMapLexpr rep inp (Function var lexpr) = if rep /= var
                                                    then (Function var mapped_lexpr)
                                                    else (Function var lexpr)
            where
                mapped_lexpr = betaMapLexpr rep inp lexpr

        betaMapLexpr rep inp (Application lexpr1 lexpr2) = (Application lexpr1' lexpr2')
            where
                lexpr1' = betaMapLexpr rep inp lexpr1
                lexpr2' = betaMapLexpr rep inp lexpr2

-- traceStack ("Calling betaReduction\n" ++ prettyCallStack callStack) $

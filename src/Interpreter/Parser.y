{
module Interpreter.Parser (parser) where
import Interpreter.Lexer
import Data.Char (isDigit, isLetter, isAlphaNum, isSpace)
import Interpreter.Common
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  ')'       { TLParen }      
  '('       { TRParen }
  '->'      { TArrow }        
  Slash     { TSlash }     
  Var       { TVar $$ }
%%

ExprList  : ExprList Function               {$1 ++ [$2]}
          | ExprList Var                    {$1 ++ [(Var $2)]}
          | Function                        {[$1]}
          | Var                             {[(Var $1)]}

Function  : '(' Slash Var '->' ExprList ')' {Function $3 $5}

{

parseError :: [Token] -> a
parseError t = error $ "Parse error: " ++ (show t)


}


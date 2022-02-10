{
module Parser (parser) where
import Lexer 
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
  VarName   { TVar }
%%

ExprList  : ExprList Function               {$2:$1}
          | ExprList Var                    {$2:$1}
          | Function                        {[$1]}
          | Var                             {[$1]}

Function  : '(' Bindings '->' ExprList ')'  {Function $2 $4}

Bindings  : Bindings Slash Var              {$3:$1}
          | Slash Var                       {[$2]}

Var       : VarName                         {Var $1}

{

parseError :: [Token] -> a
parseError t = error $ "Parse error: " ++ (show t)


}


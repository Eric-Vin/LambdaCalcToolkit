{
module Compiler.Parser (parser) where
import Compiler.Lexer 
import Data.Char (isDigit, isLetter, isAlphaNum, isSpace)
import Compiler.Common
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  true      { TTrue }       
  false     { TFalse }     
  if        { TIf }         
  then      { TThen }      
  else      { TElse }      
  while     { TWhile }      
  do        { TDo }  
  skip      { TSkip}
  ';'       { TSemi }       
  ':='      { TDef }       
  '='       { TEq }         
  '+'       { TPlus }       
  '-'       { TMinus }      
  '*'       { TMult }       
  '('       { TOParen }     
  ')'       { TCParen }    
  '{'       { TOBrac }      
  '}'       { TCBrac }      
  '<'       { TLess }      
  '~'       { TComp }       
  '&'       { TLand }       
  '|'       { TLor }   
  Var       { TVar $$ } 
  Int       { TInt $$ }
%%

Command : Command ';' CAtom {Semi $1 $3}
        | CAtom             {$1}

CAtom   : skip              {Skip}
        | Var ':=' AExpr    {Assign $1 $3}
        | if BExpr 
          then CAtom 
          else CAtom        {If $2 $4 $6}
        | while BExpr 
          do CAtom          {While $2 $4}
        | '{' Command '}'   {$2}


BExpr   : LogOp             {$1}

LogOp   : LogOp '&' Neg     {And $1 $3}
        | LogOp '|' Neg     {Or $1 $3}
        | Neg               {$1}

Neg     : '~' Neg           {Not $2}
        | BAtom             {$1}

BAtom   : true              {Boolean True}
        | false             {Boolean False}
        | AExpr '<' AExpr   {Less $1 $3}
        | AExpr '=' AExpr   {Eq $1 $3}
        | '(' BExpr ')'     {$2}


AExpr   : Sum              {$1}


Sum     : Sum '+' Prod      {Add $1 $3}
        | Sum '-' Prod      {Sub $1 $3}
        | Prod              {$1}

Prod    : Prod '*' Unary    {Mult $1 $3}
        | Unary             {$1}

Unary   : '-' AAtom         {Neg $2}
        | AAtom             {$1}

AAtom   : Var               {Variable $1}
        | Int               {Number $1}
        | '(' AExpr ')'     {$2}

{

parseError :: [Token] -> a
parseError t = error $ "Parse error: " ++ (show t)


}


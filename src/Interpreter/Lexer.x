{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$letter = [a-zA-Z]

tokens :-

  $white+                   ;
  \(                        {\_ -> TLParen}
  \)                        {\_ -> TRParen}
  \-\>                      {\_ -> TArrow}
  \\                        {\_ -> TSlash}
  $letter [$letter $digit]* {\s -> TVar s }

{
data Token 
  = TLParen       
  | TRParen      
  | TArrow         
  | TSlash       
  | TVar       
  deriving (Eq,Show)

lexer = alexScanTokens
}

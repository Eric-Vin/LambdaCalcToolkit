{
module Compiler.Lexer where
}

%wrapper "basic"

$digit = 0-9
$letter = [a-zA-Z]

tokens :-

  $white+                   ;
  true                      {\_ -> TTrue}
  false                     {\_ -> TFalse}
  if                        {\_ -> TIf}
  then                      {\_ -> TThen}
  else                      {\_ -> TElse}
  while                     {\_ -> TWhile}
  do                        {\_ -> TDo}
  skip                      {\_ -> TSkip}
  input                     {\_ -> TInput}
  print                     {\_ -> TPrint}
  \;                        {\_ -> TSemi}
  \,                        {\_ -> TComma}
  \:\=                      {\_ -> TDef}
  \=                        {\_ -> TEq}
  \+                        {\_ -> TPlus}
  \-                        {\_ -> TMinus}
  \*                        {\_ -> TMult}
  \(                        {\_ -> TOParen}
  \)                        {\_ -> TCParen}
  \{                        {\_ -> TOBrac}
  \}                        {\_ -> TCBrac}
  \<                        {\_ -> TLess}
  \¬                        {\_ -> TComp}
  \∧                        {\_ -> TLand}
  \∨                        {\_ -> TLor}
  $digit+                   {\s -> TInt (read s)}
  $letter [$letter $digit]* {\s -> TVar s }

{
data Token 
  = TTrue       
  | TFalse      
  | TIf         
  | TThen       
  | TElse       
  | TWhile      
  | TDo
  | TSkip
  | TInput
  | TPrint  
  | TSemi       
  | TComma
  | TDef        
  | TEq         
  | TPlus       
  | TMinus      
  | TMult       
  | TOParen     
  | TCParen     
  | TOBrac      
  | TCBrac      
  | TLess       
  | TComp       
  | TLand       
  | TLor    
  | TVar String 
  | TInt Int
  deriving (Eq,Show)

lexer = alexScanTokens
}

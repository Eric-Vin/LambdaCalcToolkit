

-- Lambda Calc Datatypes

type LambdaVar = String

data LambdaExpr = Function [LambdaVar] [LambdaExpr]
                | Var LambdaVar

-- Encoding Helper Functions
encodeInt :: Int -> Int -> LambdaExpr
encodeInt c i   = undefined

encodeBool :: Int -> Int -> Bool -> LambdaExpr
encodeBool c b  = undefined

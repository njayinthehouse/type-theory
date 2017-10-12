
module Evaluator where

data Ast = TRUE
         | FALSE
         | ZERO
         | IfThenElse Ast Ast Ast
         | Succ Ast
         | Pred Ast
         | IsZero Ast
         | ERROR
         deriving Show

isValue :: Ast -> Bool
isValue TRUE = True
isValue FALSE = True
isValue ZERO = True
isValue ERROR = True
isValue (Succ n) = isNumVal n
isValue t = False

isNumVal :: Ast -> Bool
isNumVal ZERO = True
isNumVal (Succ n) = isNumVal n
isNumVal t = False

eval :: Ast -> Ast
eval TRUE = TRUE
eval FALSE = FALSE
eval ZERO = ZERO
eval (IfThenElse TRUE t2 t3) = eval t2
eval (IfThenElse FALSE t2 t3) = eval t3
eval (IfThenElse t1 t2 t3) = if (isValue t1)
                             then ERROR
                             else eval (IfThenElse (eval t1) t2 t3)
eval (Succ t) | isNumVal t = Succ t
              | isValue t = ERROR
              | otherwise = eval (Succ (eval t))
eval (Pred (Succ t)) =  eval t
eval (Pred ZERO) = ZERO
eval (Pred t) | isValue t = ERROR
              | otherwise = eval (Pred (eval t))
eval (IsZero ZERO) = TRUE
eval (IsZero (Succ t)) | isNumVal t = FALSE
                       | otherwise = eval (IsZero (eval (Succ t)))
eval (IsZero t) | isValue t = ERROR
                | otherwise = eval (IsZero (eval t))
eval t = ERROR

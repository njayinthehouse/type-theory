
module Lambda (isValue, fv, (->:), (|-), alpha, beta, eval)
where

import Prelude hiding ((*))
import Data.Set

validIdentifiers :: Set Char
validIdentifiers = fromList (['a'..'z'] ++ ['A'..'Z'])

data Ast = Variable Char
         | Lambda Char Ast
         | Application Ast Ast
         deriving (Eq, Show)

(\-) :: Char -> Ast -> Ast
(\-) x t = Lambda x t

(*) :: Ast -> Ast -> Ast
(*) t1 t2 = Application t1 t2

var :: Char -> Ast
var x = Variable x

isValue :: Ast -> Bool
isValue (Variable _) = True
isValue (Lambda _ _) = True
isValue (Application (Lambda _ _) _) = False
isValue (Application t1 t2) = (isValue t1) && (isValue t2)

fv :: Ast -> Set Char
fv (Variable x) = singleton x
fv (Lambda x t) = delete x (fv t)
fv (Application x y) = (fv x) `union` (fv y)

data Substitution = Sub Ast Ast

(->:) :: Ast -> Ast -> Substitution
(->:) x y = Sub x y

toChar :: Ast -> Char
toChar (Variable x) = x
toChar _ = error "Cannot convert nonvariable to Char!"

(|-) :: Substitution -> Ast -> Ast
(|-) (Sub x s) (Variable a) | b == a = s
                            | otherwise = var b
  where b = toChar x
        
(|-) (Sub x s) (Lambda y t) | a == y = (\-) y t
                            | y `notMember` (fv s) = (\-) y ((x ->: s) |- t)
                            | otherwise = (x ->: s) |- (((\-) y t) `alpha` z)
  where a = toChar x
        z = findMin (validIdentifiers \\ ((fv s) `union` (fv t)))
        
(|-) (Sub x s) (Application t1 t2) = let q = x ->: s
                                     in (q |- t1) * (q |- t2)

alpha :: Ast -> Char -> Ast
alpha (Lambda a t) b = let x = var a
                           y = var b
                       in (\-) b ((x ->: y)|- t)
alpha _ _ = error "Alpha conversion on nonabstraction!" 

beta :: Ast -> Ast
beta (Application (Lambda a t) s) = let x = var a
                                    in (x ->: s)|- t

eval :: Ast -> Ast
eval (Application t1 t2) | not (isValue t1) = eval ((eval t1) * t2)
                         | not (isValue t2) = eval (t1 * (eval t2))
                         | otherwise = case t1 of
                                         Lambda _ _ -> eval (beta (t1 * t2))
                                         _ -> t1 * t2
eval ast = ast

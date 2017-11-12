
import Data.List

data Type = Boolean
          | Any
          | Function Type Type
          deriving (Eq, Show)

type Identifier = String

data Term = Var Identifier
          | Abs Identifier Type Term
          | App Term Term
          | PrimTrue
          | PrimFalse
          | IfThenElse Term Term Term
          deriving (Eq, Show)

data DeBrujinTerm = Index Int 
                  | Lambda Identifier Type DeBrujinTerm
                  | Application DeBrujinTerm DeBrujinTerm
                  | True'
                  | False'
                  | If DeBrujinTerm DeBrujinTerm DeBrujinTerm
                  deriving (Eq, Show)

fv :: Term -> [Identifier]
fv (Var x) = [x]
fv (Abs x q t) = (fv t) \\ [x]
fv (App s t) = (fv s) `union` (fv t)
fv (IfThenElse r s t) = (fv r) `union` (fv s) `union` (fv t)
fv _ = []

type Context = [(Identifier, Type)]

indexOf :: Identifier -> Context -> Int
indexOf x context = case (findIndex (\y -> fst y == x) context) of
                      Just n -> n
                      Nothing -> error "Identifier does not exist in context!"

getType :: Int -> Context -> Type
getType i context = snd (context !! i)

getIdentifier :: Int -> Context -> Identifier
getIdentifier i context = fst (context !! i)

convert :: Term -> (DeBrujinTerm, Context)
convert x =
  let
    aux :: Context -> Term -> DeBrujinTerm
    aux context (Var x) = Index (indexOf x context)
    aux context (Abs x q t) = Lambda x q (aux ((x, q) : context) t)
    aux context (App s t) = let convert' = aux context
                            in Application (convert' s) (convert' t)
    aux _ PrimTrue = True'
    aux _ PrimFalse = False'
    aux context (IfThenElse r s t) = let convert' = aux context
                                     in If (convert' r) (convert' s) (convert' t)

    buildContext :: Term -> Context
    buildContext x = map (\x -> (x, Any)) (fv x)

    context :: Context
    context = buildContext x
  in
    (aux context x, context)

convertBack :: Context -> DeBrujinTerm -> Term
convertBack context (Index k) = Var (getIdentifier k context)
convertBack context (Lambda x q t) = Abs x q (convertBack ((x, q) : context) t)
convertBack context (Application s t) = App (convertBack context s) (convertBack context t)
convertBack _ True' = PrimTrue
convertBack _ False' = PrimFalse
convertBack context (If t1 t2 t3) = let convert' = convertBack context
                                    in IfThenElse (convert' t1) (convert' t2) (convert' t3)

(-^-) :: Int -> Int -> (DeBrujinTerm -> DeBrujinTerm)
(d -^- c) (Index k)
  | k < c = Index k
  | otherwise = Index (k + d)
(d -^- c) (Lambda x q t) = Lambda x q ((d -^- (c + 1)) t)
(d -^- c) (Application s t) = let shift = d -^- c
                              in Application (shift s) (shift t)
(_ -^- _) literal = literal

(-:>) :: Int -> DeBrujinTerm -> (DeBrujinTerm -> DeBrujinTerm)
(i -:> s) (Index k)
  | i == k = s
  | otherwise = Index k
(i -:> s) (Lambda x q t) = Lambda x q (((i + 1) -:> s) ((1-^-1) t))
(i -:> s) (Application r t) = Application ((i -:> s) r) ((i -:> s) t)
(_ -:> _) literal = literal

isValue :: DeBrujinTerm -> Bool
isValue (Application (Lambda _ _ _) _) = False
isValue (Application x y) = (isValue x) && (isValue y)
isValue (If _ _ _) = False
isValue _ = True

typeOf :: Term -> Type
typeOf term =
  let
    aux :: DeBrujinTerm -> Context -> Type
    aux (Index k) context = getType k context
    aux (Lambda x q t) context =
      let
        context' = (x, q) : context
        q' = aux t context'
      in
        Function q q'
    aux (Application t1 t2) context =
      let
        q1 = aux t1 context
        q2 = aux t2 context
      in
        case q1 of
          Function q11 q12 -> if (q11 == q2)
                              then q12
                              else error "Incompatible types in application!"
          _ -> error "Ill-formed application!"
    aux True' _ = Boolean
    aux False' _ = Boolean
    aux (If t1 t2 t3) context=
      if (aux t1 context == Boolean)
      then let q2 = aux t2 context
               q3 = aux t3 context
           in if (q2 == q3)
              then q2
              else error "Branches of the if statement are of different types!"
      else error "Type mismatch: Boolean expected!"
                                 
    (t, context) = convert term
  in
    aux t context

evaluate :: Term -> (Term, Type)
evaluate term =
  let
    aux :: DeBrujinTerm -> DeBrujinTerm
    aux (Application s t)
      | not (isValue s) = Application (aux s) t
      | not (isValue t) = Application s (aux t)
      | otherwise = case s of
                      Lambda x q e -> ((-1)-^-0) ((0 -:> ((1-^-0) t)) e)
                      _ -> Application s t
    aux (If r s t)
      | predicate == True' = aux s
      | predicate == False' = aux s
      | otherwise = error "Non-boolean in test position!"
      where predicate = aux r
    aux x = x

    (t, context) = convert term
  in
    let result = convertBack context (aux t)
    in (result, typeOf result)

module Ast where

data Exp
  = Var Ident           
  | App Exp Exp   
  | Lam Ident Exp 
  | Con Value
  deriving (Show, Eq)
             
data Value
  = Num Int
  | Add
  deriving (Show, Eq)

newtype Ident
  = Ident String deriving (Show, Eq)

mkNum :: Int -> Exp
mkNum n = Con (Num n)

mkAdd :: Exp -> Exp -> Exp
mkAdd e1 e2 = (App (App (Con Add) e1) e2)

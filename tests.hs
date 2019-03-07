module Tests where

import Prelude hiding (exp,fail)
import Result
import Ast
import qualified Syntax

tests :: [IO ()]
tests =
  [
    check "" (Err "syntax error at 1"),
    check " " (Err "syntax error at 2"),
    check "#" (Err "syntax error at 1"),
    check "4#" (Err "unconsumed input at 2"),
    check "4x" (Err "syntax error at 2"),
    check "42x" (Err "syntax error at 3"),

    check "4" (Ok (Con (Num 4))),
    check "42" (Ok (Con (Num 42))),
    check "4 " (Ok (Con (Num 4))),
    check " 4" (Ok (Con (Num 4))),

    check "x" (Ok (Var (Ident "x"))),
    check "xy" (Ok (Var (Ident "xy"))),
    check "x4" (Ok (Var (Ident "x4"))),
    check "x y" (Ok (App (Var (Ident "x")) (Var (Ident "y")))),
    check "  x  y  " (Ok (App (Var (Ident "x")) (Var (Ident "y")))),
    check "x y z" (Ok (App (App (Var (Ident "x")) (Var (Ident "y"))) (Var (Ident "z")))),
    check "x 4" (Ok (App (Var (Ident "x")) (Con (Num 4)))),
    check "x 4 y" (Ok (App (App (Var (Ident "x")) (Con (Num 4))) (Var (Ident "y")))),
    check "4 y" (Ok (App (Con (Num 4)) (Var (Ident "y")))), -- type silly (application of a number!) but syntactically fine

    check ")" (Err "syntax error at 1"),
    check "(" (Err "syntax error at 2"),
    check "()" (Err "syntax error at 2"),
    check "4)" (Err "unconsumed input at 2"),
    
    check "(4)" (Ok (Con (Num 4))),
    check " ( 4 ) " (Ok (Con (Num 4))),
    check "((4))" (Ok (Con (Num 4))),
    check "x (y)" (Ok (App (Var (Ident "x")) (Var (Ident "y")))),
    check "(x) y" (Ok (App (Var (Ident "x")) (Var (Ident "y")))),
    check "(x) (y)" (Ok (App (Var (Ident "x")) (Var (Ident "y")))),
    check "((x) (y))" (Ok (App (Var (Ident "x")) (Var (Ident "y")))),

    check "(x y) z" (Ok (App (App (Var (Ident "x")) (Var (Ident "y"))) (Var (Ident "z")))),
    check "x (y z)" (Ok (App (Var (Ident "x")) (App (Var (Ident "y")) (Var (Ident "z"))))),

    check "\\x.x" (Ok (Lam (Ident "x") (Var (Ident "x")))),
    check " \\ x . x " (Ok (Lam (Ident "x") (Var (Ident "x")))),
    check "\\x.\\y.x 1" (Ok (Lam (Ident "x") (Lam (Ident "y") (App (Var (Ident "x")) (Con (Num 1)))))),
    check "\\x.(\\y.x) 1" (Ok (Lam (Ident "x") (App (Lam (Ident "y") (Var (Ident "x"))) (Con (Num 1))))),
    check "(\\x.\\y.x) 1" (Ok (App (Lam (Ident "x") (Lam (Ident "y") (Var (Ident "x")))) (Con (Num 1)))),
    check "f \\x.x" (Ok (App (Var (Ident "f")) (Lam (Ident "x") (Var (Ident "x"))))),

    check "1+2" (Ok (mkAdd (mkNum 1) (mkNum 2))),
    check " 1 + 2 " (Ok (mkAdd (mkNum 1) (mkNum 2))),
    check "(1+2)+3" (Ok (mkAdd (mkAdd (mkNum 1) (mkNum 2)) (mkNum 3))),
    check "1+(2+3)" (Ok (mkAdd (mkNum 1) (mkAdd (mkNum 2) (mkNum 3)))),
    check "1+2+3" (Ok (mkAdd (mkAdd (mkNum 1) (mkNum 2)) (mkNum 3))),
    
    check "(\\f.\\x.f(f x))(\\x.x+1)5" (Ok sampleExp),
    check " ( \\ f . \\ x . f ( f x ) ) ( \\ x . x + 1 ) 5 " (Ok sampleExp)
  ]

sampleExp :: Exp -- my canonical sample expression
sampleExp = App (App twice inc) (Con (Num 5)) where
  twice = Lam f (Lam x (App (Var f) (App (Var f) (Var x))))
  inc  = Lam x (App (App (Con Add) (Var x)) (Con (Num 1)))
  f = Ident "f"
  x = Ident "x"

check :: String -> Result Exp -> IO ()
check s expect =
  if (actual == expect)
  then putStr ("PASS: " ++ show s ++ "\n")
  else putStr ("FAIL: " ++ show s ++ "\n--Expect = " ++ show expect ++ "\n--Actual = " ++ show actual ++ "\n")
  where actual = Syntax.parseExp s 

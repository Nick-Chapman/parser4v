module Syntax (parseExp) where

import Prelude hiding (exp)
import qualified Data.Char as Char
import Result
import Ast
import qualified Par4c as Par  -- use Par4c *or* Par4v, here..
import Par4c (Par,alt,satisfy) -- and here

parseExp :: String -> Result Exp
parseExp s = Par.exec topExp s

-- syntax for the expression language
             
topExp :: Par Exp
topExp = do
  whitespace
  exp

exp :: Par Exp
exp =
  binaryAdd -- TODO: generalize to other binops

binaryAdd :: Par Exp
binaryAdd = do
  e1 <- application
  es <- many (do symbol '+' ; application)
  return (foldl mkAdd e1 es)

application :: Par Exp
application = do
  func <- atom
  args <- many atom
  return (foldl App func args)

atom :: Par Exp
atom = alts [
  abstraction,
  variable,
  number,
  parenthenized exp
  ]

abstraction :: Par Exp
abstraction = do
  symbol '\\'
  x <- ident
  symbol '.'
  e <- exp
  return (Lam x e)

variable :: Par Exp
variable = do
  x <- ident
  return (Var x)

ident :: Par Ident
ident = nibble (do
  x1 <- sat Char.isAlpha
  xs <- many (sat (\c -> Char.isAlphaNum c || c == '_'))
  return (Ident (x1:xs)))

number :: Par Exp
number = nibble (do
  c <- sat Char.isDigit 
  n <- numberAcc (digitOfChar c)
  mustNotParse (sat Char.isAlpha)
  return (Con (Num n)))
  where
    numberAcc :: Int -> Par Int
    numberAcc acc = alts [
      pure acc,
      do
        c <- sat Char.isDigit
        numberAcc (acc * 10 + digitOfChar c)
      ]
    digitOfChar :: Char -> Int
    digitOfChar c = Char.ord c - ord0
      where ord0 = Char.ord '0'

-- utility parsers & combinators

mustNotParse :: Par a -> Par ()
mustNotParse par = alt (pure ()) (do _ <- Par.nonConsuming par; Par.error)

parenthenized :: Par a -> Par a
parenthenized par = do
  symbol '('
  x <- par
  symbol ')'
  return x

symbol :: Char -> Par ()
symbol c = nibble (satisfy (\d -> if c==d then Just () else Nothing))

nibble :: Par a -> Par a -- consume trailing whitespace
nibble par = do
  a <- par
  whitespace
  return a

whitespace :: Par ()
whitespace = skipWhile (satisfy (\c -> if Char.isSpace c then Just () else Nothing))

many :: Par a -> Par [a] -- argument parser must always consume!
many par = knot where
  knot = alt (pure []) (do x <- par; xs <- knot; return (x:xs))

alts :: [Par a] -> Par a
alts = foldl alt Par.fail

sat :: (Char -> Bool) -> Par Char
sat pred = satisfy (\c -> if pred c then Just c else Nothing)

skipWhile :: Par () -> Par ()
skipWhile par = knot where knot = alt (pure ()) (do par; knot)

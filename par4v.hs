module Par4v (Par,exec,fail,alt,error,nonConsuming,satisfy) where -- 4-way variant style

import Prelude hiding (error,fail)
import Control.Monad(liftM, ap)
import Result

instance Functor Par where
  fmap = liftM

instance Applicative Par where
  pure = unit
  (<*>) = ap

instance Monad Par where
  (>>=) = bind

-- the parser implementation
    
newtype Pos = Pos { charNum :: Int }

nextPos :: Pos -> Pos
nextPos (Pos n) = Pos (n+1)

data Outcome a
  = Succ a Pos String
  | Eps a
  | Fail
  | Error Pos

data Par a = Par { run :: Pos -> String -> Outcome a }

exec :: Par a -> String -> Result a
exec par xs =
  case run par q xs of
   Error q     -> syntaxError q
   Fail      -> syntaxError q
   Succ a q xs -> checkConsumed a q xs
   Eps a     -> checkConsumed a q xs
  where
    q = Pos 1
    syntaxError q = Err ("syntax error at " ++ show (charNum q))
    checkConsumed a q xs =
      case xs of
       [] -> Ok a
       _:_ -> Err ("unconsumed input at " ++ show (charNum q))

unit :: a -> Par a
unit a = Par (\_ _ -> Eps a)

fail :: Par a
fail = Par (\_ _ -> Fail)

error :: Par a
error = Par (\pos _ -> Error pos)

nonConsuming :: Par a -> Par a
nonConsuming par =
  Par (\pos s -> case run par pos s of
        Error _ -> Fail
        Succ a _ _ -> Eps a
        Fail -> Fail
        Eps a -> Eps a)

alt :: Par a -> Par a -> Par a
alt par1 par2 =
  Par (\pos s -> case run par1 pos s of
        Error pos -> Error pos
        Succ a pos s -> Succ a pos s
        Fail -> run par2 pos s
        Eps a -> case run par2 pos s of
          Fail -> Eps a
          Eps _ -> Eps a
          r -> r)

bind :: Par a -> (a -> Par b) -> Par b
bind p f =
  Par (\pos s -> case run p pos s of
        Error e -> Error e
        Fail -> Fail
        Eps a -> run (f a) pos s
        Succ a pos s ->
          case run (f a) pos s of
           Fail -> Error pos
           Eps a -> Succ a pos s
           r -> r)

satisfy :: (Char -> Maybe a) -> Par a
satisfy f =
  Par (\pos s -> case s of
        [] -> Fail
        x:xs -> case f x of Just x -> Succ x (nextPos pos) xs ; Nothing -> Fail)

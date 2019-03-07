{-#LANGUAGE ExplicitForAll, Rank2Types #-}

module Par4c (Par,exec,fail,alt,error,nonConsuming,satisfy) where -- 4-continuation style

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

newtype Par a =
  Par { run :: forall res.
           Pos                          -- q
        -> String                       -- xs
        -> (a -> Pos -> String -> res)  -- s
        -> (a -> res)                   -- p
        -> (Pos -> res)                 -- e
        -> res                          -- f
        -> res
      } 

exec :: Par a -> String -> Result a
exec par xs = run par q xs s p e f
  where
    q        = Pos 1
    s a q xs = checkConsumed a q xs
    p a      = checkConsumed a q xs
    e q      = syntaxError q
    f        = syntaxError q
    syntaxError q = Err ("syntax error at " ++ show (charNum q))
    checkConsumed a q xs =
      case xs of
       [] -> Ok a
       _:_ -> Err ("unconsumed input at " ++ show (charNum q))

unit :: a -> Par a
unit a = Par (\_ _ _ p _ _ -> p a)

fail :: Par a
fail = Par (\_ _ _ _ _ f -> f)

error :: Par a
error = Par (\q _ _ _ e _ -> e q)

nonConsuming :: Par a -> Par a
nonConsuming par = Par (\q xs _ p _ f -> run par q xs (\a _ _ -> p a) p (\_ -> f) f)

alt :: Par a -> Par a -> Par a
alt par1 par2 =
  Par (\q xs s p e f ->
        run par1 q xs
        s
        (\a -> run par2 q xs s (\_ -> p a) e (p a)) -- prefer left-Eps over right-Eps/Fail
        e
        (run par2 q xs s p e f))

bind :: Par a -> (a -> Par b) -> Par b
bind par1 func =
  Par (\q xs s p e f ->
        run par1 q xs
        (\a q xs -> run (func a) q xs s (\a -> s a q xs) e (e q)) -- consume! (x2 places)
        (\a -> run (func a) q xs s p e f)
        e
        f)
  
satisfy :: (Char -> Maybe a) -> Par a
satisfy func =
  Par (\q xs s _ _ f -> case xs of
        [] -> f
        x:xs -> case func x of Just y -> s y (nextPos q) xs ; Nothing -> f)

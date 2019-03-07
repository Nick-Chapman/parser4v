module Result where

data Result a = Ok a | Err String deriving (Show,Eq) -- what is the idiomatic Haskell for this?

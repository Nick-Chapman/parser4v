module Main where

import Control.Exception
import Tests

main :: IO ()
main = runAll Tests.tests

runAll :: [IO ()] -> IO ()
runAll xs =  do
  _ <- sequence (map wrap xs)
  return ()

wrap :: IO () -> IO ()
wrap = handle (\e -> putStr ("EXCEPTION: " ++ show (e::SomeException) ++ "\n"))

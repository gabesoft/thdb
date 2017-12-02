module Main where

import Engine
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  repl stdin

repl :: Handle -> IO ()
repl stdin = do
  cmd <- readCmd stdin
  case cmd of
    END -> return ()
    _ -> run cmd >> repl stdin

readCmd :: Handle -> IO Command
readCmd handle = read <$> hGetLine handle

run :: Command -> IO ()
run (GET name) = putStrLn name
run (SET name value) = putStrLn $ name ++ " " ++ value
run (UNSET name) = putStrLn name
run (NUMEQUALTO value) = putStrLn value
run BEGIN = putStrLn "begin"
run ROLLBACK = putStrLn "rollback"
run COMMIT = putStrLn "commit"

exit = undefined
module Main where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Map.Strict as M
import Engine
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  void $ runStateT (repl stdin) Db { values = M.empty, stack = [] }

repl :: Handle -> StateT Db IO ()
repl stdin = do
  cmd <- liftIO $ readCmd stdin
  case cmd of
    END -> return ()
    _ -> do
      out <- exec cmd
      case out of
        Just output -> liftIO $ putStrLn output
        Nothing -> return ()
      repl stdin

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
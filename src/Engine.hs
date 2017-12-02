-- ^
-- Database engine
module Engine where

import Control.Monad.Trans.State
import Data.Map.Strict as M
import Prelude as P

data Command =
    SET String String
  | GET String
  | UNSET String
  | NUMEQUALTO String
  | BEGIN
  | ROLLBACK
  | COMMIT
  | END
  deriving (Eq, Read, Show)

data Db = Db {
      values:: Map String String,
      stack:: [ Map String String ]
    }

-- ^
-- Execute a database command
exec :: Monad m => Command -> StateT Db m (Maybe String)
exec (GET name) = do
  values <- gets values
  return $ case M.lookup name values of
             Just x -> Just x
             Nothing -> Just "NULL"
exec (SET name value) = do
  db <- get
  put $ db { values = M.insert name value $ values db }
  return Nothing
exec (UNSET name) = do
  db <- get
  put $ db { values = M.delete name $ values db}
  return Nothing
exec (NUMEQUALTO name) = do
  values <- gets values
  return $ Just $ show (count name values)
exec BEGIN = do
  db <- get
  put $ db { stack = values db : stack db }
  return Nothing
exec ROLLBACK = do
  db <- get
  let st = stack db
  if P.null st
    then return (Just "NO TRANSACTION")
    else do
        put Db { stack = tail st, values = head st }
        return Nothing
exec COMMIT = do
  db <- get
  let st = stack db
  if P.null st
     then return (Just "NO TRANSACTION")
     else do
       put $ db { stack = [] }
       return Nothing

-- ^
-- Count the entries with the given value
count :: String -> Map String String -> Integer
count value = M.foldr go 0
  where go v c
           | v == value = c + 1
           | otherwise = c

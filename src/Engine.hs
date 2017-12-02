-- ^
-- Database engine
module Engine where

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


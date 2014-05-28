module SpecHelper where

-- haskell platform libraries
import Text.Parsec.Error

-- foreign libraries
import Test.Hspec

instance Eq ParseError where
  e1 == e2 = errorPos e1 == errorPos e2 && errorMessages e1 == errorMessages e2

newLine :: String
newLine = "\r\n"

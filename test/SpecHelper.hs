module SpecHelper where

-- haskell platform libraries
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.String

-- foreign libraries
import Test.Hspec

type TestParser a = Either ParseError a

instance Eq ParseError where
  e1 == e2 = errorPos e1 == errorPos e2 &&
    errorMessages e1 == errorMessages e2

parseWith :: Parser a -> String -> Either ParseError a
parseWith parser str = parse parser str $ str ++ "\r\n"

shouldParseTo :: (Show a, Eq a, Show b, Eq b) =>
  Either a b -> b -> Expectation
x `shouldParseTo` y = x `shouldBe` Right y

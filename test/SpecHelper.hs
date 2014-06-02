module SpecHelper
( TestParser
, parseWith
, parseLineWith
, parseLinesWith
, shouldParseTo
, stubParser
) where

-- haskell platform libraries
import Data.List (foldl')
import Text.Parsec.Char
import Text.Parsec.Combinator
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
parseWith parser str = parse parser str str

parseLineWith :: Parser a -> String -> Either ParseError a
parseLineWith parser str = parse parser str $ str ++ lineBreak

parseLinesWith :: Parser a -> [String] -> Either ParseError a
parseLinesWith parser strs = parse parser str str
  where
    str = foldl' (\x xs -> x ++ xs ++ lineBreak) [] strs

shouldParseTo ::
  (Show a, Eq a, Show b, Eq b) =>
  Either a b -> b -> Expectation
x `shouldParseTo` y = x `shouldBe` Right y

stubParser :: Parser String
stubParser = manyTill anyChar $ string lineBreak

lineBreak :: String
lineBreak = "\r\n"

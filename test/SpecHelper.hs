module SpecHelper
( TestParser
, fromLines
, parseWith
, parseLineWith
, parseLinesWith
, shouldFail
, shouldParseTo
, shouldSucceed
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

fromLines :: [String] -> String
fromLines = foldl' (\x xs -> x ++ xs ++ lineBreak) []

parseWith :: Parser a -> String -> TestParser a
parseWith parser str = parse parser str str

parseLineWith :: Parser a -> String -> TestParser a
parseLineWith parser str = parse parser str $ str ++ lineBreak

parseLinesWith :: Parser a -> [String] -> TestParser a
parseLinesWith parser strs = parse parser str str
  where
    str = fromLines strs

shouldFail :: (Show a) => TestParser a -> Expectation
shouldFail parsed = parsed `shouldSatisfy` either (const True) (const False)

shouldSucceed :: (Show a) => TestParser a -> Expectation
shouldSucceed parsed = parsed `shouldSatisfy` either (const False) (const True)

shouldParseTo :: (Show a, Eq a) => TestParser a -> a -> Expectation
x `shouldParseTo` y = x `shouldBe` Right y

stubParser :: Parser String
stubParser = manyTill anyChar $ string lineBreak

-- private functions

lineBreak :: String
lineBreak = "\r\n"

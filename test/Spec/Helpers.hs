module Spec.Helpers
( parseLineWith
, parseLinesWith
, parseWith
, stubParser
) where

-- haskell platform libraries
import Data.List (foldl')
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.String

type TestParser a = Either ParseError a

parseWith :: Parser a -> String -> TestParser a
parseWith parser str = parse parser str str

parseLineWith :: Parser a -> String -> TestParser a
parseLineWith parser str = parseWith parser $ str ++ lineBreak

parseLinesWith :: Parser a -> [String] -> TestParser a
parseLinesWith parser = parseWith parser . concatMap (++ lineBreak)

stubParser :: Parser String
stubParser = manyTill anyChar $ string lineBreak

-- private functions

lineBreak :: String
lineBreak = "\r\n"

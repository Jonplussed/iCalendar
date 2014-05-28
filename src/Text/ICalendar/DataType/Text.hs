module Text.ICalendar.DataType.Text
( asText
) where

import Data.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim

import Text.ICalendar.Parser.Combinator

asText :: Parser String
asText = do
  strings <- sepBy1 (manyTill anyChar lineBreak) space
  return $ unwords strings

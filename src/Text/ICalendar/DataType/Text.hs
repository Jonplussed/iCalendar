module Text.ICalendar.DataType.Text
( textType
) where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim

import Text.ICalendar.Parser.Combinator

textType :: Parser String
textType = do
  strings <- sepBy1 (manyTill anyChar lineBreak) space
  return $ unwords strings

module Text.ICalendar.DataType.Text ( toText ) where

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char

import Text.ICalendar.Parser.Combinator

toText :: Parser String
toText = manyTill anyChar newLine >>= return

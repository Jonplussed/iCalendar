module Text.ICalendar.Type.Text ( toText ) where

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char

import Text.ICalendar.Type.All

toText :: Parser ICalData
toText = many1 anyChar >>= return . ICalText

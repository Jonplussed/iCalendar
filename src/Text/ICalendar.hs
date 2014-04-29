module Text.ICalendar
( ICalendar
, fromFile
, fromString
) where

import Text.ParserCombinators.Parsec
import Text.ICalendar.Parser.Combinators

type ICalendar = Either ParseError ICalMap

fromFile :: String -> IO ICalendar
fromFile path = readFile path >>= return . fromString

fromString :: String -> ICalendar
fromString text = parse iCalendar "iCalendar" text

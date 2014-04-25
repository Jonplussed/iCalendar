module Text.ICalendar
( ICalendar
--, VCalendar(..)
--, VEvent(..)
, fromFile
, fromString
) where

import Text.ParserCombinators.Parsec
import Text.ICalendar.Parsec.Combinators

type ICalendar = Either ParseError ICalMap

fromFile :: String -> IO ICalendar
fromFile path = readFile path >>= return . fromString

fromString :: String -> ICalendar
fromString text = parse iCalendar "iCalendar" text

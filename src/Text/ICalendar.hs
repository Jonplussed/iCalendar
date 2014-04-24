module Text.ICalendar
( ICalendar
--, VCalendar(..)
--, VEvent(..)
, fromFile
, fromString
) where

import Text.ParserCombinators.Parsec
import Text.ICalendar.Parsec.Combinators
-- import Text.ICalendar.Component.VCalendar ( VCalendar(..), vCalendar )
-- import Text.ICalendar.Component.VEvent ( VEvent(..) )

type ICalendar = Either ParseError ICalMap

fromFile :: String -> IO ICalendar
fromFile path = readFile path >>= return . fromString

fromString :: String -> ICalendar
fromString text = parse iCalendar "iCalendar" text

module Text.ICalendar
( ICalendar
, VCalendar(..)
, VEvent(..)
, fromFile
, fromString
) where

import Text.ParserCombinators.Parsec
import Text.ICalendar.VCalendar ( VCalendar(..), vCalendar )
import Text.ICalendar.VEvent ( VEvent(..) )

type ICalendar = Either ParseError VCalendar

fromFile :: String -> IO ICalendar
fromFile path = readFile path >>= return . fromString

fromString :: String -> ICalendar
fromString text = parse vCalendar "iCalendar" text

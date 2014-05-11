module Text.ICalendar
( fromFile
, fromString
) where

import Text.Parsec
import Text.Parsec.Error

import Text.ICalendar.Parser (iCalendar)
import Text.ICalendar.Component.VCalendar (VCalendar)

type ICalendar = Either ParseError VCalendar

fromFile :: String -> IO ICalendar
fromFile path = readFile path >>= return . fromString

fromString :: String -> ICalendar
fromString text =  parse iCalendar "iCalendar" text

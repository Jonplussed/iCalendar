module Text.ICalendar
( ICalendar
, fromFile
, fromString
) where

import Text.Parsec.Error
import Text.Parsec

import Text.ICalendar.Component.VCalendar
import Text.ICalendar.Parser.Combinators
import Text.ICalendar.Parser.Validators

type ICalendar = Either ParseError VCalendar

fromFile :: String -> IO ICalendar
fromFile path = readFile path >>= return . fromString

fromString :: String -> ICalendar
fromString text =  iCalTree >>= iCalRoot
  where
    iCalTree = parse iCalendar "iCalendar" text
    iCalRoot = reqComp1 "VCALENDAR" vCalendar

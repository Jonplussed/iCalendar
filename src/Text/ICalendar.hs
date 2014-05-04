module Text.ICalendar
( fromFile
, fromString
) where

import Text.Parsec (parse)

import Text.ICalendar.Component.VCalendar
import Text.ICalendar.Parser.Combinator
import Text.ICalendar.Parser.Validator

fromFile :: String -> IO (ICalendar VCalendar)
fromFile path = readFile path >>= return . fromString

fromString :: String -> ICalendar VCalendar
fromString text =  iCalTree >>= iCalRoot
  where
    iCalTree = parse iCalendar "iCalendar" text
    iCalRoot = reqComp1 "VCALENDAR" vCalendar

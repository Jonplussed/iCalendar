module Text.ICal
( VCalendar(..)
, VEvent(..)
, parseICal
) where

import Text.ParserCombinators.Parsec
import Text.ICal.VCalendar ( VCalendar(..), vCalendar )
import Text.ICal.VEvent ( VEvent(..) )

parseICal :: String -> VCalendar
parseICal text =
  case parse vCalendar "vCalendar" text of
    Left error -> undefined
    Right result -> result

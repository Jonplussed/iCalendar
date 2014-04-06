module Text.ICal (parseICal) where

import Text.ParserCombinators.Parsec
import Text.ICal.VCalendar (vCalendar)

parseICal :: String -> String
parseICal text =
  case parse vCalendar "vCalendar" text of
    Left error -> "Couldn't parse: " ++ show error
    Right result -> "It worked!"

module Text.ICalendar.Component.VCalendar
( VCalendar(..)
, vCalendar
) where

import Text.ParserCombinators.Parsec
import Text.ICalendar.Parsec.Combinators
import Text.ICalendar.Component.VEvent ( VEvent, vEvent )

data VCalendar = VCalendar { prodid   :: Maybe String
                           , version  :: Maybe String
                           , calscale :: Maybe String
                           , method   :: Maybe String
                           , vEvents  :: [VEvent]
                           , warnings :: [String] } deriving (Eq, Show)

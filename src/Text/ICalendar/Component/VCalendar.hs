module Text.ICalendar.Component.VCalendar
( VCalendar(..)
, vCalendar
) where

import Text.ParserCombinators.Parsec
import Text.ICalendar.Parser.Validators
--import Text.ICalendar.Component.VEvent ( VEvent, vEvent )

data VCalendar = VCalendar { prodid   :: String
                           , version  :: String
                           , calscale :: Maybe String
                           , method   :: Maybe String
                           --, vEvents  :: [VEvent]
                           }

vCalendar :: ICalTree -> VCalendar
vCalendar tree = fn tree $ do
  prodid    <- something
  version   <- somethingElse
  calscale  <- iHaveNoIdea
  method    <- whatever

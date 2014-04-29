{-# LANGUAGE RecordWildCards   #-}

module Text.ICalendar.Component.VCalendar
( VCalendar(..)
, vCalendar
) where

import Text.ParserCombinators.Parsec
import Text.ICalendar.Parser.Combinators
import Text.ICalendar.Parser.Validators
--import Text.ICalendar.Component.VEvent ( VEvent, vEvent )

data VCalendar = VCalendar { prodid   :: String
                           , version  :: String
                           , calscale :: Maybe String
                           , method   :: Maybe String
                           --, vEvents  :: [VEvent]
                           }

vCalendar :: ICalTree -> Either ValidationError VCalendar
vCalendar tree = do
  prodid    <- req1 "prodid" tree
  version   <- req1 "version" tree
  calscale  <- opt1 "calscale" tree
  method    <- opt1 "method" tree
  return VCalendar {..}

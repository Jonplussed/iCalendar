{-# LANGUAGE RecordWildCards   #-}

module Text.ICalendar.Component.VCalendar
( VCalendar(..)
, vCalendar
) where

import Text.Parsec.Error

import Text.ICalendar.Parser.Combinators
import Text.ICalendar.Parser.Validators
--import Text.ICalendar.Component.VEvent ( VEvent, vEvent )

data VCalendar = VCalendar { prodid   :: String
                           , version  :: String
                           , calscale :: Maybe String
                           , method   :: Maybe String
                           --, vEvents  :: [VEvent]
                           } deriving (Eq, Show)

vCalendar :: ICalTree -> Either ParseError VCalendar
vCalendar tree = do
  prodid    <- reqProp1 "PRODID"    tree
  version   <- reqProp1 "VERSION"   tree
  calscale  <- optProp1 "CALSCALE"  tree
  method    <- optProp1 "METHOD"    tree
  return VCalendar {..}

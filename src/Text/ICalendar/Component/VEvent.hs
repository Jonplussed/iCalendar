{-# LANGUAGE RecordWildCards #-}

module Text.ICalendar.Component.VEvent
( VEvent(..)
, vEvent
) where

import Text.Parsec.Error

import Text.ICalendar.Parser.Combinators
import Text.ICalendar.Parser.Validators

data VEvent = VEvent { dtstart      :: String
                     , attendee     :: [String]
                     , uid          :: Maybe String
                     , duration     :: Maybe String
                     , organizer    :: Maybe String
                     , location     :: Maybe String
                     , summary      :: Maybe String
                     , description  :: Maybe String
                     , transp       :: Maybe String
                     } deriving (Eq, Show)

vEvent :: ICalTree -> Either ParseError VEvent
vEvent tree = do
  dtstart     <- reqProp1 "DTSTART"     tree
  attendee    <- optPropN "ATTENDEE"    tree
  uid         <- optProp1 "UID"         tree
  duration    <- optProp1 "DURATION"    tree
  organizer   <- optProp1 "ORGANIZER"   tree
  location    <- optProp1 "LOCATION"    tree
  summary     <- optProp1 "SUMMARY"     tree
  description <- optProp1 "DESCRIPTION" tree
  transp      <- optProp1 "TRANSP"      tree
  return VEvent {..}

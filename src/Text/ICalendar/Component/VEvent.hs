{-# LANGUAGE RecordWildCards #-}

module Text.ICalendar.Component.VEvent
( VEvent(..)
, vEvent
) where

import Control.Applicative ((<$>))
import Data.Time
import Text.Parsec.Error (ParseError)

import Text.ICalendar.Parser.Combinator
import Text.ICalendar.Parser.Validator

data Interval = Duration NominalDiffTime
              | EndDate UTCTime

data VEvent = VEvent { startDate    :: String
                     , attendees    :: [String]
                     , uniqueId     :: Maybe String
                     , organizer    :: Maybe String
                     , location     :: Maybe String
                     , summary      :: Maybe String
                     , description  :: Maybe String
                     , transparency :: String
                     , interval     :: Interval
                     } deriving (Eq, Show)

vEvent :: ICalTree -> ICalendar VEvent
vEvent tree = do
    startDate     <- reqProp1   "DTSTART"          tree
    attendees     <- optPropN   "ATTENDEE"         tree
    uniqueId      <- optProp1   "UID"              tree
    duration      <- optProp1   "DURATION"         tree
    organizer     <- optProp1   "ORGANIZER"        tree
    location      <- optProp1   "LOCATION"         tree
    summary       <- optProp1   "SUMMARY"          tree
    description   <- optProp1   "DESCRIPTION"      tree
    transparency  <- optProp1   "TRANSP"           tree
    interval      <- reqCoProp1 "DURATION" "DTEND" tree
    return VEvent {..}

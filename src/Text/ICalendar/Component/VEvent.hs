{-# LANGUAGE RecordWildCards #-}

module Text.ICalendar.Component.VEvent
( VEvent(..)
, vEvent
) where

import Control.Applicative ((<$>))
import Text.Parsec.Error (ParseError)

import Text.ICalendar.Parser.Combinators
import Text.ICalendar.Parser.Validators

data Transparency = Transparent
                  | Opaque
                  deriving (Eq, Show)

data VEvent = VEvent { startDate    :: String
                     , attendees    :: [String]
                     , uniqueId     :: Maybe String
                     , duration     :: Maybe String
                     , organizer    :: Maybe String
                     , location     :: Maybe String
                     , summary      :: Maybe String
                     , description  :: Maybe String
                     , transparency :: Transparency
                     } deriving (Eq, Show)

vEvent :: ICalTree -> ICalendar VEvent
vEvent tree = do
    startDate     <-              reqProp1 "DTSTART"     tree
    attendees     <-              optPropN "ATTENDEE"    tree
    uniqueId      <-              optProp1 "UID"         tree
    duration      <-              optProp1 "DURATION"    tree
    organizer     <-              optProp1 "ORGANIZER"   tree
    location      <-              optProp1 "LOCATION"    tree
    summary       <-              optProp1 "SUMMARY"     tree
    description   <-              optProp1 "DESCRIPTION" tree
    transparency  <- toTransp <$> optProp1 "TRANSP"      tree
    return VEvent {..}

-- private functions

toTransp :: Maybe String -> Transparency
toTransp str = if str == Just "TRANSPARENT"
               then Transparent
               else Opaque

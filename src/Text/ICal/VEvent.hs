module Text.ICal.VEvent (vEvent) where

import Text.ParserCombinators.Parsec
import Text.ICal.Parsec.Combinators

vEvent :: Parser [String]
vEvent = component "vevent" parseVEvent

--
-- private functions
--

parseVEvent :: Parser [String]
parseVEvent = many1 . choice $ props ++ comps
  where
    props = optionalProps properties
    comps = []

properties :: [String]
properties = [ "attendee"
             , "description"
             , "dtstart"
             , "duration"
             , "location"
             , "organizer"
             , "summary"
             , "transp"
             , "uid"
             ]

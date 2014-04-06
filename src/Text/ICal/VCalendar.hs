module Text.ICal.VCalendar (vCalendar) where

import Text.ParserCombinators.Parsec
import Text.ICal.Parsec.Combinators
import Text.ICal.VEvent (vEvent)

vCalendar :: Parser ()
vCalendar = component "vcalendar" (props >> comps) >> eof
  where
    props = many1 . choice $ optionalProps properties
    comps = many1 vEvent

--
-- private functions
--

properties :: [String]
properties = [ "prodid"
             , "version"
             , "calscale"
             , "method"
             ]

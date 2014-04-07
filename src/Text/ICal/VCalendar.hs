module Text.ICal.VCalendar ( vCalendar ) where

import Text.ParserCombinators.Parsec
import Text.ICal.Parsec.Combinators
import Text.ICal.VEvent ( VEvent(..), vEvent )

data VCalendar = VCalendar { prodid   :: String
                           , version  :: String
                           , calscale :: String
                           , method   :: String
                           , vEvents  :: [VEvent] } deriving (Eq, Show)

vCalendar :: Parser [[String]]
vCalendar = do
    tree <- component "vcalendar" (props >> comps)
    eof
    return tree
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

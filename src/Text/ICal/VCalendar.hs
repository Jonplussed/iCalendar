module Text.ICal.VCalendar
( VCalendar(..)
, vCalendar
) where

import Text.ParserCombinators.Parsec
import Text.ICal.Parsec.Combinators
import Text.ICal.VEvent ( VEvent, vEvent )

data VCalendar = VCalendar { prodid   :: String
                           , version  :: String
                           , calscale :: String
                           , method   :: String
                           , vEvents  :: [VEvent] } deriving (Eq, Show)

vCalendar :: Parser VCalendar
vCalendar = do
    cal <- component "vcalendar" (assignAttrs empty attributes) id
    eof
    return cal

empty :: VCalendar
empty = VCalendar "" "" "" "" []

attributes :: [Parser (VCalendar -> VCalendar)]
attributes = [ property  "prodid"         $ \v c -> c { prodid = v }
             , property  "version"        $ \v c -> c { version = v }
             , property  "calscale"       $ \v c -> c { calscale = v }
             , property  "method"         $ \v c -> c { method = v }
             , component "vevent" vEvent  $ \v c -> c { vEvents = vEvents c ++ [v] }
             ]

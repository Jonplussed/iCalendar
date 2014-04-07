module Text.ICal.VEvent
( VEvent(..)
, vEvent
) where

import Text.ParserCombinators.Parsec
import Text.ICal.Parsec.Combinators

data VEvent = VEvent { attendee    :: String
                     , description :: String
                     , dtstart     :: String
                     , duration    :: String
                     , location    :: String
                     , organizer   :: String
                     , summary     :: String
                     , transp      :: String
                     , uid         :: String } deriving (Eq, Show)

vEvent :: Parser VEvent
vEvent = component "vevent" $ attrs
  where
    attrs = do
      fs <- many1 $ choice attributes
      return . foldr1 (.) fs $ newVEvent

--
-- private functions
--

newVEvent :: VEvent
newVEvent = VEvent "" "" "" "" "" "" "" "" ""

attributes :: [Parser (VEvent -> VEvent)]
attributes = [ property "attendee"    $ \v e -> e { attendee = v }
             , property "description" $ \v e -> e { description = v }
             , property "dtstart"     $ \v e -> e { dtstart = v }
             , property "duration"    $ \v e -> e { duration = v }
             , property "location"    $ \v e -> e { location = v }
             , property "organizer"   $ \v e -> e { organizer = v }
             , property "summary"     $ \v e -> e { summary = v }
             , property "transp"      $ \v e -> e { transp = v }
             , property "uid"         $ \v e -> e { uid = v }
             ]

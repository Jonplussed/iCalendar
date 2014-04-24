module Text.ICalendar.Component.VEvent
( VEvent(..)
, vEvent
) where

import Text.ParserCombinators.Parsec
import Text.ICalendar.Parsec.Combinators

data VEvent = VEvent { attendee    :: String
                     , description :: String
                     , dtstart     :: String
                     , duration    :: String
                     , location    :: String
                     , organizer   :: String
                     , summary     :: String
                     , transp      :: Transparency
                     , uid         :: String
                     , eWarnings    :: [String] } deriving (Eq, Show)

data Transparency = Transparent
                  | Opaque
                  deriving (Eq, Show)

vEvent :: Parser VEvent
vEvent = assignAttrs empty attributes

empty :: VEvent
empty = VEvent { attendee     = ""
               , description  = ""
               , dtstart      = ""
               , duration     = ""
               , location     = ""
               , organizer    = ""
               , summary      = ""
               , transp       = Opaque
               , uid          = ""
               , eWarnings     = []
               }

attributes :: [Parser (VEvent -> VEvent)]
attributes = [ property "attendee"    $ \v e -> e { attendee = v }
             , property "description" $ \v e -> e { description = v }
             , property "dtstart"     $ \v e -> e { dtstart = v }
             , property "duration"    $ \v e -> e { duration = v }
             , property "location"    $ \v e -> e { location = v }
             , property "organizer"   $ \v e -> e { organizer = v }
             , property "summary"     $ \v e -> e { summary = v }
             , property "transp"      $ \v e -> e { transp = transparency v }
             , property "uid"         $ \v e -> e { uid = v }
             ]

transparency :: String -> Transparency
transparency t = if t == "TRANSPARENT" then Transparent else Opaque

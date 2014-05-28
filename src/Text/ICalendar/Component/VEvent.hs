module Text.ICalendar.Component.VEvent where

-- haskell platform libraries
import Control.Applicative
import Data.Time.Clock
import Text.Parsec.String

-- foreign libraries
import Text.Parsec.Permutation

-- local libraries
import Text.ICalendar.Parser.Validator
import Text.ICalendar.DataType.Text
import Text.ICalendar.DataType.Duration

data Interval = Duration DiffTime
              | EndDate String
              deriving (Eq, Show)

data VEvent = VEvent { startDate    :: String
                     , attendees    :: [String]
                     , uniqueId     :: Maybe String
                     , organizer    :: Maybe String
                     , location     :: Maybe String
                     , summary      :: Maybe String
                     , description  :: Maybe String
                     , transparency :: Maybe String
                     , interval     :: Interval
                     } deriving (Eq, Show)

vEvent :: Parser VEvent
vEvent = runPermParser $
    VEvent <$> reqProp1 toText "dtstart"
           <*> optPropN toText "attendee"
           <*> optProp1 toText "organizer"
           <*> optProp1 toText "uid"
           <*> optProp1 toText "location"
           <*> optProp1 toText "summary"
           <*> optProp1 toText "description"
           <*> optProp1 toText "transp"
           <*> reqCoProp1
                 (Duration <$> toDuration, "duration")
                 (EndDate <$> toText, "dtend")

module Text.ICalendar.Component.VEvent
( TimeInterval (..)
, Transparency (..)
, VEvent (..)
, parseVEvent
) where

-- haskell platform libraries
import Control.Applicative
import Data.Time.Clock
import Text.Parsec.String

-- foreign libraries
import Text.Parsec.Permutation

-- local libraries
import Text.ICalendar.DataType.Duration
import Text.ICalendar.DataType.Text
import Text.ICalendar.Parser.Validator

data TimeInterval = Duration DiffTime
                  | EndDate String
                  deriving (Eq, Show)

data Transparency = Transparent
                  | Opaque
                  deriving (Eq, Show)

data VEvent = VEvent { startDate    :: String
                     , attendees    :: [String]
                     , uniqueId     :: Maybe String
                     , organizer    :: Maybe String
                     , location     :: Maybe String
                     , summary      :: Maybe String
                     , description  :: Maybe String
                     , transparency :: Transparency
                     , timeInterval :: TimeInterval
                     } deriving (Eq, Show)

parseVEvent :: Parser VEvent
parseVEvent = runPermParser $

    VEvent <$> reqProp1    textType    "DTSTART"
           <*> optPropN    textType    "ATTENDEE"
           <*> optProp1    textType    "UID"
           <*> optProp1    textType    "ORGANIZER"
           <*> optProp1    textType    "LOCATION"
           <*> optProp1    textType    "SUMMARY"
           <*> optProp1    textType    "DESCRIPTION"
           <*> transp1     textType    "TRANSP"
           <*> reqCoProp1
                          (tiDurType,  "DURATION")
                          (tiDateType, "DTEND")

  where transp1 par key = toTransp <$> optProp1 par key
        tiDurType       = Duration <$> durationType
        tiDateType      = EndDate  <$> textType

-- private functions

toTransp :: Maybe String -> Transparency
toTransp (Just "TRANSPARENT") = Transparent
toTransp _                    = Opaque

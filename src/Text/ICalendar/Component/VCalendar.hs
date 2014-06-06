module Text.ICalendar.Component.VCalendar
( VCalendar (..)
, CalScale (..)
, vCalendar
) where

-- haskell platform libraries
import Control.Applicative
import Text.Parsec.String

-- foreign libraries
import Text.Parsec.Permutation

-- native libraries
import Text.ICalendar.Parser.Validator
import Text.ICalendar.DataType.Text
import Text.ICalendar.Component.VEvent

data CalScale = Gregorian
              | Unsupported String
              deriving (Eq, Show)

data VCalendar = VCalendar { productId  :: String
                           , version    :: String
                           , scale      :: Maybe CalScale
                           , method     :: Maybe String
                           , events     :: [VEvent]
                           } deriving (Eq, Show)

vCalendar :: Parser VCalendar
vCalendar = properties >>= components
  where
    properties = runPermParser $
      VCalendar <$> reqProp1 asText                  "PRODID"
                <*> reqProp1 asText                  "VERSION"
                <*> optProp1 (toCalScale <$> asText) "CALSCALE"
                <*> optProp1 asText                  "METHOD"
    components partialVCal = runPermParser $
      partialVCal <$> optCompN vEvent "VEVENT"

-- private functions

toCalScale :: String -> CalScale
toCalScale "GREGORIAN" = Gregorian
toCalScale other = Unsupported other

module Text.ICalendar.Component.VCalendar where

-- haskell platform libraries
import Control.Applicative
import Text.Parsec.String

-- foreign libraries
import Text.Parsec.Permutation

-- local libraries
import Text.ICalendar.Parser.Validator
import Text.ICalendar.Type.Text
import Text.ICalendar.Component.VEvent

data CalendarScale = Gregorian
                   | Unsupported String
                   deriving (Eq, Show)

data VCalendar = VCalendar { productId  :: String
                           , version    :: String
                           , scale      :: Maybe String
                           , method     :: Maybe String
                           , events     :: [VEvent]
                           } deriving (Eq, Show)

vCalendar :: Parser VCalendar
vCalendar = runPermParser $
    VCalendar <$> reqProp1 toText "prodid"
              <*> reqProp1 toText "version"
              <*> optProp1 toText "calscale"
              <*> optProp1 toText "method"
              <*> optCompN vEvent "vevent"

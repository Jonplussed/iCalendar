module Text.ICalendar.Component.VCalendar
( CalScale (..)
, VCalendar (..)
, parseVCalendar
) where

-- haskell platform libraries
import Control.Applicative ((<$>), (<*>))
import Text.Parsec.String

-- foreign libraries
import Text.Parsec.Permutation

-- native libraries
import Text.ICalendar.Component.VEvent
import Text.ICalendar.DataType.Text
import Text.ICalendar.Parser.Validator

data CalScale = Gregorian
              | Unsupported String
              deriving (Eq, Show)

data VCalendar = VCalendar { productId  :: String
                           , version    :: String
                           , scale      :: CalScale
                           , method     :: Maybe String
                           , events     :: [VEvent]
                           } deriving (Eq, Show)

parseVCalendar :: Parser VCalendar
parseVCalendar = properties >>= components
  where

    properties = runPermParser $
      VCalendar <$> reqProp1 textType "PRODID"
                <*> reqProp1 textType "VERSION"
                <*> scale1   textType "CALSCALE"
                <*> optProp1 textType "METHOD"

    components vCalendar = runPermParser $
      vCalendar <$> optCompN parseVEvent "VEVENT"

    scale1 par key = toCalScale <$> optProp1 par key

-- private functions

toCalScale :: Maybe String -> CalScale
toCalScale (Just "GREGORIAN") = Gregorian
toCalScale (Just other)       = Unsupported other
toCalScale _                  = Gregorian

module Text.ICalendar.Component.VCalendar where

-- haskell platform libraries
import Control.Applicative
import Text.Parsec.String

-- imported libraries
import Text.Parsec.Permutation

-- local libraries
import Text.ICalendar.Parser.Combinator
import Text.ICalendar.Type.Text

data CalendarScale = Gregorian
                   | Unsupported String
                   deriving (Eq, Show)

data VCalendar = VCalendar { productId  :: String
                           , version    :: String
                           , scale      :: Maybe String
                           , method     :: Maybe String
                           --, events     :: [VEvent]
                           } deriving (Eq, Show)

vCalendar :: Parser VCalendar
vCalendar = runPermParser $ VCalendar <$> reqProp1 toText "prodid"
                                      <*> reqProp1 toText "version"
                                      <*> optProp1 toText "calscale"
                                      <*> optProp1 toText "method"

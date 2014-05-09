module Text.ICalendar.Type.All where

import qualified Data.Map as M
import Data.Time.Clock
import Text.Parsec.Pos

data ICalData = ICalBinary String
              | ICalBoolean String
              | ICalCalendarUserAddress String
              | ICalDate  String
              | ICalDateTime String
              | ICalDuration DiffTime
              | ICalFloat String
              | ICalInteger String
              | ICalPeriodOfTime String
              | ICalRecurrenceRule String
              | ICalText String
              | ICalTime String
              | ICalURI String
              | ICalUTCOffset String
              | ICalComponent (M.Map String [ICalData]) SourcePos
              deriving (Eq, Show)

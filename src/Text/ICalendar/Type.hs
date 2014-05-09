module Text.ICalendar.Type
( ICalData(..)
, toICalDataType
) where

import qualified Data.Map as M
import Text.Parsec.String

import Text.ICalendar.Type.All
import Text.ICalendar.Type.Duration
import Text.ICalendar.Type.Text

toICalDataType :: String -> Parser ICalData
toICalDataType key = M.findWithDefault toText key dataTypeMap

-- private functions

dataTypeMap :: M.Map String (Parser ICalData)
dataTypeMap = M.fromList [ ("DURATION", toDuration) ]

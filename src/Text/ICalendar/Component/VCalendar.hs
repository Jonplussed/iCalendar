{-# LANGUAGE RecordWildCards   #-}

module Text.ICalendar.Component.VCalendar
( VCalendar(..)
, vCalendar
) where

import qualified  Data.HashMap.Lazy   as H
import            Text.Parsec.Error
import            Text.Parsec.Pos

import Text.ICalendar.Parser.Combinators
import Text.ICalendar.Parser.Validators
--import Text.ICalendar.Component.VEvent ( VEvent, vEvent )

data VCalendar = VCalendar { prodid   :: String
                           , version  :: String
                           , calscale :: Maybe String
                           , method   :: Maybe String
                           --, vEvents  :: [VEvent]
                           } deriving (Eq, Show)

vCalendar :: ICalTree -> Either ParseError VCalendar
vCalendar tree =
  case H.lookup node tree of
    Just (Component [x] _)  -> vCalendar' x
    _                       -> err "must be the root component defined in an iCalendar file"
  where
    err msg = Left $ specError (initialPos "root") node msg
    node = "VCALENDAR"

-- private functions

vCalendar' :: ICalTree -> Either ParseError VCalendar
vCalendar' tree = do
  prodid    <- req1 "PRODID" tree
  version   <- req1 "VERSION" tree
  calscale  <- opt1 "CALSCALE" tree
  method    <- opt1 "METHOD" tree
  return VCalendar {..}

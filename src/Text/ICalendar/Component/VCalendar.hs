{-# LANGUAGE RecordWildCards #-}

module Text.ICalendar.Component.VCalendar
( VCalendar(..)
, vCalendar
) where

import Control.Applicative ((<$>))
import Text.Parsec.Error (ParseError)

import Text.ICalendar.Parser.Combinators
import Text.ICalendar.Parser.Validators
import Text.ICalendar.Component.VEvent

data CalendarScale = Gregorian
                   | Unsupported
                   deriving (Eq, Show)

data VCalendar = VCalendar { productId  :: String
                           , version    :: String
                           , scale      :: CalendarScale
                           , method     :: Maybe String
                           , events     :: [VEvent]
                           } deriving (Eq, Show)

vCalendar :: ICalTree -> ICalendar VCalendar
vCalendar tree = do
  productId <- reqProp1 "PRODID"        tree
  version   <- reqProp1 "VERSION"       tree
  method    <- optProp1 "METHOD"        tree
  events    <- optCompN "VEVENT" vEvent tree

  scale <- calendarScale <$> optProp1 "CALSCALE" tree
  return VCalendar {..}

-- private functions

calendarScale :: Maybe String -> CalendarScale
calendarScale s = if s == Just "GREGORIAN"
                  then Gregorian
                  else Unsupported

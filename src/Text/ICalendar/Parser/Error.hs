module Text.ICalendar.Parser.Error
( ICalendar
, req1Error
, reqNError
, opt1Error
, notCompError
, notPropError
, reqCo1Error
, reqCoNError
, validationError
) where

import Text.Parsec.Error (ParseError, Message (Message), newErrorMessage)
import Text.Parsec.Pos (SourcePos)

import Text.ICalendar.Parser.Combinator

type ICalendar a = Either ParseError a

req1Error, reqNError, opt1Error, notPropError, notCompError
  :: String -> SourcePos -> ICalendar a
req1Error n    = validationError (n ++ " must be assigned once")
reqNError n    = validationError (n ++ " must be assigned at least once")
opt1Error n    = validationError (n ++ " cannot be assigned more than once")
notPropError n = validationError (n ++ " must be an ICalendar property")
notCompError n = validationError (n ++ " must be an ICalendar component")

reqCo1Error, reqCoNError
  :: String -> String -> SourcePos -> ICalendar a
reqCo1Error n1 n2 = validationError ("either only " ++ n1 ++ " or " ++ n2 ++ " must be assigned once")
reqCoNError n1 n2 = validationError ("either only " ++ n1 ++ " or " ++ n2 ++ " must be assigned at least once")

validationError :: String -> SourcePos -> ICalendar a
validationError msg = Left . newErrorMessage (Message $ "invalid ICalendar file: " ++ msg)

module Text.ICalendar.Parser.Validators where

import Data.Char (toUpper)
import qualified Data.HashMap.Lazy as H
import Text.Parsec.Error

import Text.ICalendar.Parser.Combinators

type ValidationError = String

opt1 :: String -> ICalMap -> Either ValidationError (Maybe String)
opt1 node tree =
  case H.lookup (upcase node) tree of
    Just (Property [x]) -> Right $ Just x
    Nothing             -> Right Nothing
    x                   -> possibleErrors node x

optN :: String -> ICalMap -> Either ValidationError [String]
optN node tree =
  case H.lookup (upcase node) tree of
    Just (Property x) -> Right x
    Nothing           -> Right []
    x                 -> possibleErrors node x

req1 :: String -> ICalMap -> Either ValidationError String
req1 node tree =
  case H.lookup (upcase node) tree of
    Just (Property [x]) -> Right x
    x                   -> possibleErrors node x

reqN :: String -> ICalMap -> Either ValidationError [String]
reqN node tree =
  case H.lookup (upcase node) tree of
    Just (Property all@(x:xs))  -> Right all
    x                           -> possibleErrors node x

-- private functions

possibleErrors :: String -> Maybe ICalParam -> Either ValidationError a
possibleErrors node value =
  case value of
    Nothing            -> err "must be assigned at least once"
    Just (Property _)  -> err "cannot be assigned more than once"
    Just _             -> err "is expected to be a property"
  where
    err msg = Left $ node ++ msg

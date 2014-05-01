module Text.ICalendar.Parser.Validators where

import            Data.Char (toUpper)
import qualified  Data.HashMap.Lazy   as H
import            Text.Parsec.Error
import            Text.Parsec.Pos

import Text.ICalendar.Parser.Combinators

type CompBuilder a = (ICalTree -> Either ParseError a)

reqComp1 :: String -> CompBuilder a -> ICalTree -> Either ParseError a
reqComp1 node builder tree =
  case H.lookup node tree of
    Just (Component [x] _)  -> builder x
    x                       -> componentErrors node x

optCompN :: String -> CompBuilder a -> ICalTree -> Either ParseError [a]
optCompN node builder tree =
  case H.lookup node tree of
    Just (Component x _)  -> sequence $ map builder x
    Nothing               -> Right []
    x                     -> componentErrors node x

optProp1 :: String -> ICalTree -> Either ParseError (Maybe String)
optProp1 node tree =
  case H.lookup node tree of
    Just (Property [x] _) -> Right $ Just x
    Nothing               -> Right Nothing
    x                     -> propertyErrors node x

optPropN :: String -> ICalTree -> Either ParseError [String]
optPropN node tree =
  case H.lookup node tree of
    Just (Property x _) -> Right x
    Nothing             -> Right []
    x                   -> propertyErrors node x

reqProp1 :: String -> ICalTree -> Either ParseError String
reqProp1 node tree =
  case H.lookup node tree of
    Just (Property [x] _) -> Right x
    x                     -> propertyErrors node x

reqPropN :: String -> ICalTree -> Either ParseError [String]
reqPropN node tree =
  case H.lookup node tree of
    Just (Property all@(x:xs) _)  -> Right all
    x                             -> propertyErrors node x

specError :: SourcePos -> String -> String -> ParseError
specError pos node msg = newErrorMessage (Message fullMsg) pos
  where
    fullMsg = "invalid ICalendar file: " ++ node ++ " " ++ msg

-- private functions

componentErrors :: String -> Maybe ICalParam -> Either ParseError a
componentErrors node value =
  case value of
    -- this error position should be the openning line of the containing component
    Nothing                 -> err (initialPos "---") "must be assigned at least once"
    Just (Component _ pos)  -> err pos "cannot be assigned more than once"
    Just (Property _ pos)   -> err pos "is expected to be a component"
  where
    err pos msg = Left $ specError pos node msg

propertyErrors :: String -> Maybe ICalParam -> Either ParseError a
propertyErrors node value =
  case value of
    -- this error position should be the openning line of the containing component
    Nothing                 -> err (initialPos "---") "must be assigned at least once"
    Just (Property _ pos)   -> err pos "cannot be assigned more than once"
    Just (Component _ pos)  -> err pos "is expected to be a property"
  where
    err pos msg = Left $ specError pos node msg

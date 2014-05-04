module Text.ICalendar.Parser.Validators
( ICalendar
, reqComp1
--, optCompN
, optProp1
, reqProp1
, optPropN
, reqPropN
--, reqCoProp1
, specError
) where

import Control.Applicative
import Data.Char (toUpper)
import qualified Data.HashMap.Lazy as H
import Data.Monoid
import Text.Parsec.Error (ParseError, Message (Message), newErrorMessage)
import Text.Parsec.Pos (SourcePos, initialPos)

import Text.ICalendar.Parser.Combinators

type ICalendar a = Either ParseError a
type Builder a = ICalTree -> ICalendar a
type CoPropFn a = (String, String -> a)

instance (Monoid b) => Monoid (Either a b) where
  mempty = Right mempty
  Right x `mappend` Right y = Right $ x `mappend` y

reqComp1 :: String -> Builder a -> ICalTree -> ICalendar a
reqComp1 node builder tree =
  case H.lookup node tree of
    Just [Component x _]  -> builder x
    x                     -> componentErrors node x

optCompN :: String -> Builder a -> ICalTree -> ICalendar [a]
optCompN node builder tree =
  case H.lookup node tree of
    Just x  -> compValuesOf node builder x
    Nothing -> Right []

reqProp1 :: String -> ICalTree -> ICalendar String
reqProp1 node tree =
  case H.lookup node tree of
    Just [Property x _] -> Right x
    x                   -> propertyErrors node x

optProp1 :: String -> ICalTree -> ICalendar (Maybe String)
optProp1 node tree =
  case H.lookup node tree of
    Just []             -> Right Nothing
    Just [Property x _] -> Right $ Just x
    Nothing             -> Right Nothing
    x                   -> propertyErrors node x

reqPropN :: String -> ICalTree -> ICalendar [String]
reqPropN node tree =
  case H.lookup node tree of
    Just props@(x:xs) -> propValuesOf node props
    x                 -> propertyErrors node x

optPropN :: String -> ICalTree -> ICalendar [String]
optPropN node tree =
  case H.lookup node tree of
    Just props -> propValuesOf node props
    Nothing    -> Right []

--reqCoProp1 :: CoPropFn a -> CoPropFn a -> ICalTree -> ICalendar a
--reqCoProp1 (n1,f1) (n2,f2) tree = do
--    v1 <- optProp1 n1 tree
--    v2 <- optProp1 n2 tree
--    case (v1,v2) of
--      (Just x, Nothing) -> Right $ f1 x
--      (Nothing, Just x) -> Right $ f2 x
--      _ -> codependantPropertyErrors n1 n2 tree

specError :: SourcePos -> String -> ICalendar a
specError pos msg = Left $ newErrorMessage (Message fullMsg) pos
  where
    fullMsg = "invalid ICalendar file: " ++ msg

-- private functions

propValuesOf :: String -> [ICalParam] -> ICalendar [String]
propValuesOf node params = foldr appendValues mempty params
  where
    appendValues v vs =
      case v of
        Property v _    -> mappend vs $ Right [v]
        Component _ pos -> specError pos $ errorMsg node
    errorMsg n = "expected " ++ n ++ " to be a property"

compValuesOf :: String -> Builder a -> [ICalParam] -> ICalendar [a]
compValuesOf node builder params = foldr appendValues mempty params
  where
    appendValues v vs =
      case v of
        Property _ pos -> specError pos $ errorMsg node
        Component v _  -> mappend vs $ (\x -> [x]) <$> builder v
    errorMsg n = "expected " ++ n ++ " to be a component"

componentErrors :: String -> Maybe [ICalParam] -> ICalendar a
componentErrors node value =
  case value of
    -- this error position should be the openning line of the containing component
    Just [Component _ pos] -> err pos "cannot be assigned more than once"
    _                      -> err (initialPos "---") "must be assigned once"
  where
    err pos msg = specError pos $ node ++ " " ++ msg

propertyErrors :: String -> Maybe [ICalParam] -> ICalendar a
propertyErrors node value =
  case value of
    -- this error position should be the openning line of the containing component
    Just [Property _ pos]  -> err pos "cannot be assigned more than once"
    _                      -> err (initialPos "---") "must be assigned once"
  where
    err pos msg = specError pos $ node ++ " " ++ msg

codependantPropertyErrors :: String -> String -> ICalTree -> ICalendar a
codependantPropertyErrors n1 n2 tree =
  case (H.lookup n1 tree `mappend` H.lookup n2 tree) of
    Just [Property _ pos] -> err pos "must be assigned, but both cannot"
    -- this error position should be the openning line of the containing component
    _                     -> err (initialPos "---") "must be assigned once"
  where
    err pos msg = specError pos $ "either " ++ n1 ++ " or " ++ n2 ++ " " ++ msg

module Text.ICalendar.Parser.Validator
( ICalendar
, reqComp1
, optCompN
, optProp1
, reqProp1
, optPropN
, reqPropN
, reqCoProp1
) where

import Control.Applicative ((<$>))
import qualified Data.HashMap.Lazy as H
import Text.Parsec.Error (ParseError, Message (Message), newErrorMessage)
import Text.Parsec.Pos (SourcePos, initialPos)

import Text.ICalendar.Parser.Combinator
import Text.ICalendar.Parser.Error

type Builder a = ICalTree -> ICalendar a
type CoPropFn a = (String, String -> a)

reqComp1 :: String -> Builder a -> ICalTree -> ICalendar a
reqComp1 node builder tree =
    case H.lookup node tree of
      Just [x]     -> compValue node builder x
      Just (_:x:_) -> req1Error node $ paramPos x
      _            -> req1Error node noPosition

optCompN :: String -> Builder a -> ICalTree -> ICalendar [a]
optCompN node builder tree =
    case H.lookup node tree of
      Just x -> compValues node builder x
      _      -> return []

reqProp1 :: String -> ICalTree -> ICalendar String
reqProp1 node tree =
    case H.lookup node tree of
      Just [x]     -> propValue node x
      Just (_:x:_) -> req1Error node $ paramPos x
      _            -> req1Error node noPosition

optProp1 :: String -> ICalTree -> ICalendar (Maybe String)
optProp1 node tree =
    case H.lookup node tree of
      Just [x]     -> return <$> propValue node x
      Just (_:x:_) -> opt1Error node $ paramPos x
      _            -> return Nothing

reqPropN :: String -> ICalTree -> ICalendar [String]
reqPropN node tree =
    case H.lookup node tree of
      Just all@(_:_) -> propValues node all
      _              -> reqNError node noPosition

optPropN :: String -> ICalTree -> ICalendar [String]
optPropN node tree =
    case H.lookup node tree of
      Just x  -> propValues node x
      Nothing -> return []

reqCoProp1 :: CoPropFn a -> CoPropFn a -> ICalTree -> ICalendar a
reqCoProp1 (n1,f1) (n2,f2) tree = do
    v1 <- optProp1 n1 tree
    v2 <- optProp1 n2 tree
    case (v1,v2) of
      (Just x, Nothing) -> return $ f1 x
      (Nothing, Just x) -> return $ f2 x
      -- I need to rework this to persist the SourcePos
      _                 -> reqCo1Error n1 n2 noPosition

-- private functions

propValue :: String -> ICalParam -> ICalendar String
propValue node param =
    case param of
      Property v _    -> return v
      Component _ pos -> notPropError node pos

propValues :: String -> [ICalParam] -> ICalendar [String]
propValues node = sequence . map (propValue node)

compValue :: String -> Builder a -> ICalParam -> ICalendar a
compValue node builder param =
    case param of
      Property _ pos -> notCompError node pos
      Component v _  -> builder v

compValues :: String -> Builder a -> [ICalParam] -> ICalendar [a]
compValues node builder = sequence . map (compValue node builder)

-- this is a placeholder until a way to persist the position of the
-- openning tag of the containing component is determined
noPosition :: SourcePos
noPosition = initialPos "---"

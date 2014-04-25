module Text.ICalendar.Parsec.Combinators where

import Data.Char (toUpper, toLower)
import Text.ParserCombinators.Parsec
import qualified Data.HashMap.Lazy as H

type ICalMap = H.HashMap String [Param]
type ICalMapFn = ICalMap -> ICalMap

data Param = Property String
           | Component ICalMap
           deriving (Eq, Show)

iCalendar :: Parser ICalMap
iCalendar = do
    params <- component
    eof
    return $ params H.empty

-- private functions

component :: Parser ICalMapFn
component = do
    string "BEGIN:"
    key <- manyTill upper newLine
    fs <- manyTill (component <|> property) (string $ "END:" ++ key)
    newLine
    update key (Component $ foldr1 (.) fs H.empty)

newLine :: Parser String
newLine = string "\r\n"

property :: Parser ICalMapFn
property = do
    key <- manyTill upper $ char ':'
    --segments <- sepBy1 (many1 anyChar) (newLine >> space)
    segments <- manyTill anyChar newLine
    update key (Property $ segments)

update :: String -> Param -> Parser ICalMapFn
update key param = return $ H.insertWith (++) (map toLower key) [param]

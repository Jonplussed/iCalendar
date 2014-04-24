module Text.ICalendar.Parsec.Combinators where

import Data.Char (toUpper, toLower)
import Text.ParserCombinators.Parsec
import qualified Data.HashMap.Lazy as H

type ICalMap = H.HashMap String [Param]

data Param = Property String
           | Component ICalMap
           deriving (Eq, Show)

iCalendar :: Parser ICalMap
iCalendar = do
    cal <- component
    eof
    return $ cal H.empty

-- property :: Parser (ICalMap -> ICalMap)
-- property = do
--     tag <- manyTill upper (try $ char ':')
--     lines <- sepBy1 (many1 anyChar) contentBreak
--     newLine
--     return $ H.insertWith (++) (downcase tag) [Property $ unwords lines]

property :: Parser (ICalMap -> ICalMap)
property = do
    tag <- manyTill upper (try $ char ':')
    lines <- many1 anyChar
    newLine
    return $ H.insertWith (++) (downcase tag) [Property $ lines]

component :: Parser (ICalMap -> ICalMap)
component = do
    string "BEGIN:"
    tag <- manyTill upper newLine
    fs <- manyTill (component <|> property) (string $ "END:" ++ tag)
    newLine
    return $ H.insertWith (++) (downcase tag) [Component $ apply fs]
  where
    apply fs = foldr1 (.) fs H.empty

newLine :: Parser String
newLine = string "\r\n"

contentBreak :: Parser Char
contentBreak = newLine >> space

upcase :: String -> String
upcase = map toUpper

downcase :: String -> String
downcase = map toLower

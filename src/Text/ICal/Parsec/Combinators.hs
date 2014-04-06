module Text.ICal.Parsec.Combinators
( ICalParser
, component
, optionalProps
, property
) where

import Data.Char (toUpper)
import Text.ParserCombinators.Parsec

type ICalParser = Parser String

component :: String -> Parser a -> Parser a
component name parser = between open close parser
  where open  = string ("BEGIN:" ++ name') >> newLine
        close = string ("END:" ++ name') >> newLine
        name' = upcase name

optionalProps :: [String] -> [Parser String]
optionalProps = map (try . property)

property :: String -> ICalParser
property name = try $ tag >> content
  where tag = string $ upcase name ++ ":"

--
-- private functions
--

content :: ICalParser
content = manyTill anyChar $ try endContent

endContent :: Parser ()
endContent = newLine >> notFollowedBy space

newLine :: ICalParser
newLine = string "\r\n"

upcase :: String -> String
upcase = map toUpper

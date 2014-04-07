module Text.ICal.Parsec.Combinators
( component
, property
) where

import Data.Char (toUpper)
import Text.ParserCombinators.Parsec

component :: String -> Parser a -> Parser a
component name parser = between open close parser
  where
    open  = string ("BEGIN:" ++ name') >> newLine
    close = string ("END:" ++ name') >> newLine
    name' = upcase name

property :: String -> (String -> a) -> Parser a
property name f = try $ tag >> content >>= return . f
  where
    tag = string $ upcase name ++ ":"

--
-- private functions
--

content :: Parser String
content = manyTill anyChar . try $ newLine >> notFollowedBy space

newLine :: Parser String
newLine = string "\r\n"

upcase :: String -> String
upcase = map toUpper

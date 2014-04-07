module Text.ICal.Parsec.Combinators
( assignAttrs
, component
, property
) where

import Data.Char (toUpper)
import Text.ParserCombinators.Parsec

assignAttrs :: a -> [Parser (a -> a)] -> Parser a
assignAttrs record parsers = do
    fs <- many1 $ choice parsers
    return . foldr (.) id fs $ record

component :: String -> Parser a -> (a -> b) -> Parser b
component name parser f = between open close parser >>= return . f
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

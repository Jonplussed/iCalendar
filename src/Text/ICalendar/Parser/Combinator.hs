module Text.ICalendar.Parser.Combinator where

import Data.Char
import Text.Parsec.String
import Text.Parsec.Char

import Text.Parsec.Permutation

newLine :: Parser String
newLine = string "\r\n"

property :: Parser a -> String -> Parser a
property typeParser key = do
    string $ map toUpper key ++ ":"
    typeParser

component :: String -> Parser a -> Parser a
component key parser = do
    string $ "BEGIN:" ++ map toUpper key
    newLine
    properties <- parser
    string $ "END:" ++ map toUpper key
    newLine
    return properties

reqProp1 typeParser k = oncePerm $ property typeParser k
optProp1 typeParser k = optionMaybePerm $ property typeParser k

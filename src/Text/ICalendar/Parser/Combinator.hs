module Text.ICalendar.Parser.Combinator
( component
, coProperty
, lineBreak
, property
) where

-- haskell platform libraries
import Data.Char
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec

component :: Parser a -> String -> Parser a
component compParser key = between open close compParser
  where
    compLine e = string (e ++ ":" ++ key) >> lineBreak
    open = try $ compLine "BEGIN"
    close = compLine "END"

coProperty :: (Parser a, String) -> (Parser a, String) -> Parser a
coProperty (typeParser1, key1) (typeParser2, key2) =
    property typeParser1 key1 <|> property typeParser2 key2

lineBreak :: Parser String
lineBreak = string "\r\n"

property :: Parser a -> String -> Parser a
property typeParser key = do
    try . string $ key ++ ":"
    typeParser

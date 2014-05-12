module Text.ICalendar.Parser.Combinator where

-- haskell platform libraries
import Data.Char
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec

newLine :: Parser ()
newLine = string "\r\n" >> notFollowedBy space

property :: Parser a -> String -> Parser a
property typeParser key =
    try $ do
      string $ map toUpper key ++ ":"
      typeParser

coProperty :: (Parser a, String) -> (Parser a, String) -> Parser a
coProperty (typeParser1, key1) (typeParser2, key2) =
    property typeParser1 key1 <|> property typeParser2 key2

component :: Parser a -> String -> Parser a
component compParser key = do
    compLine "begin"
    comp <- compParser
    compLine "end"
    return comp
  where
    compLine e = do
        string . map toUpper $ e ++ ":" ++ key
        newLine

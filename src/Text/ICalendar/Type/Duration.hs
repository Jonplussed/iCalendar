module Text.ICalendar.Type.Duration ( toDuration ) where

import Data.Time.Clock
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim

import Text.ICalendar.Parser.Combinator

toDuration :: Parser DiffTime
toDuration = do
    sign <- option '+' $ char '+' <|> char '-'
    char 'P'
    weeks <- interval 'W'
    days <- interval 'D'
    char 'T'
    hours <- interval 'H'
    minutes <- interval 'M'
    seconds <- interval 'S'
    newLine
    return $ toDiffTime sign weeks days hours minutes seconds

-- private functions

interval :: Char -> Parser Integer
interval i = option "0" (manyTill digit $ char i) >>= return . read

toDiffTime :: Char -> Integer -> Integer -> Integer -> Integer -> Integer -> DiffTime
toDiffTime sign weeks days hours minutes seconds =
    secondsToDiffTime $ multiplier * totalSeconds
  where
    multiplier   = case sign of
                     '-' -> -1
                     _   -> 1
    totalSeconds = weeks   * 604800 +
                   days    * 86400  +
                   hours   * 3600   +
                   minutes * 60     +
                   seconds

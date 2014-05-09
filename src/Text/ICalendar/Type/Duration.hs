module Text.ICalendar.Type.Duration ( toDuration ) where

import Data.Time.Clock
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim

import Text.ICalendar.Type.All
import Text.ICalendar.Parser.Error

toDuration :: Parser ICalData
toDuration = do
    sign <- try $ char '+' <|> char '-'
    char 'P'
    weeks <- interval 'W'
    days <- interval 'D'
    char 'T'
    hours <- interval 'H'
    minutes <- interval 'M'
    seconds <- interval 'S'
    return . ICalDuration $ toDiffTime weeks days hours minutes seconds

-- private functions

interval :: Char -> Parser Integer
interval i = option "0" (manyTill digit $ char i) >>= return . read

toDiffTime :: Integer -> Integer -> Integer -> Integer -> Integer -> DiffTime
toDiffTime weeks days hours minutes seconds =
    secondsToDiffTime $ weeks   * 604800 +
                        days    * 86400  +
                        hours   * 3600   +
                        minutes * 60     +
                        seconds

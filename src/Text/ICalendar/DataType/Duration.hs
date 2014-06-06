module Text.ICalendar.DataType.Duration
( asDuration
) where

import Control.Applicative ((<$>))
import Data.Time.Clock
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim

import Text.ICalendar.Parser.Combinator

asDuration :: Parser DiffTime
asDuration = do
    sign <- toSign <$> option '+' (char '+' <|> char '-')
    char 'P'
    totalSecs <- durDateTime <|> durWeek
    lineBreak
    return . secondsToDiffTime $ sign totalSecs

-- private functions

durDateTime :: Parser Integer
durDateTime = do
    daySecs  <- 86400 `secondsPer` interval 'D'
    char 'T'
    hourSecs <- 3600  `secondsPer` interval 'H'
    minSecs  <- 60    `secondsPer` interval 'M'
    secs     <- 1     `secondsPer` interval 'S'
    return $ daySecs + hourSecs + minSecs + secs

durWeek :: Parser Integer
durWeek = 604800 `secondsPer` interval 'W'

interval :: Char -> Parser Integer
interval i = option "0" (try . manyTill digit $ char i) >>= return . read

secondsPer :: Integer -> (Parser Integer -> Parser Integer)
secondsPer mult = (<$>) (* mult)

toSign :: Char -> (Integer -> Integer)
toSign '-' = (*) (-1)
toSign _   = id

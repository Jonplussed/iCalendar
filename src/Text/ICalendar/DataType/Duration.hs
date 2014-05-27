module Text.ICalendar.DataType.Duration
( toDuration
) where

import Control.Applicative ((<$>))
import Data.Time.Clock
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim

import Text.ICalendar.Parser.Combinator

toDuration :: Parser DiffTime
toDuration = do
    sign <- toSign <$> option '+' (char '+' <|> char '-')
    char 'P'
    totalSecs <- durDate <|> durTime <|> durWeek
    return . secondsToDiffTime $ sign totalSecs

-- private functions

durDate :: Parser Integer
durDate = do
    daySecs <- try $ 86400 `secondsPer` interval 'D'
    timeSecs <- durTime
    return $ daySecs + timeSecs

durTime :: Parser Integer
durTime = do
    char 'T'
    hourSecs <- 3600 `secondsPer` interval 'H'
    minSecs  <- 60   `secondsPer` interval 'M'
    secs     <- 1    `secondsPer` interval 'S'
    return $ hourSecs + minSecs + secs

durWeek :: Parser Integer
durWeek = 604800 `secondsPer` interval 'W'

interval :: Char -> Parser Integer
interval i = manyTill digit (char i) >>= return . read

secondsPer :: Integer -> (Parser Integer -> Parser Integer)
secondsPer mult = (<$>) (* mult)

toSign :: Char -> (Integer -> Integer)
toSign '-' = (*) (-1)
toSign _   = id

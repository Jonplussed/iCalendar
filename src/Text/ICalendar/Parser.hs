module Text.ICalendar.Parser
( parseICalendar
) where

-- haskell platform libraries
import Text.Parsec
import Text.Parsec.String

-- local libraries
import Text.ICalendar.Parser.Combinator
import Text.ICalendar.Component.VCalendar

parseICalendar :: Parser VCalendar
parseICalendar = do
    cal <- component parseVCalendar "VCALENDAR"
    eof
    return cal

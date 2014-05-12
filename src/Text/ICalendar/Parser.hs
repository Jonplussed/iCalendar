module Text.ICalendar.Parser
( iCalendar
) where

-- haskell platform libraries
import Text.Parsec
import Text.Parsec.String

-- local libraries
import Text.ICalendar.Parser.Combinator
import Text.ICalendar.Component.VCalendar

iCalendar :: Parser VCalendar
iCalendar = do
    cal <- component vCalendar "vcalendar"
    eof
    return cal

module Text.ICalendar.Parser.Combinators
( ICalParam (..)
, ICalTree
, iCalendar
) where

import Data.Monoid
import qualified Data.HashMap.Lazy as H
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Pos

data ICalParam = Property String SourcePos
               | Component ICalTree SourcePos
               deriving (Eq, Show)

type ICalTree = H.HashMap String [ICalParam]
type ICalTreeS = ICalTree -> ICalTree

iCalendar :: Parser ICalTree
iCalendar = do
    params <- component
    eof
    return $ params H.empty

-- private functions

component :: Parser ICalTreeS
component = do
    pos <- getPosition
    string "BEGIN:"
    key <- manyTill upper newLine
    fs <- manyTill (component <|> property) (string $ "END:" ++ key)
    newLine
    update key $ Component (foldr1 (.) fs H.empty) pos

newLine :: Parser String
newLine = string "\r\n"

property :: Parser ICalTreeS
property = do
    pos <- getPosition
    key <- manyTill upper $ char ':'
    --segments <- sepBy1 (many1 anyChar) (newLine >> space)
    segments <- manyTill anyChar newLine
    update key $ Property segments pos

update :: String -> ICalParam -> Parser ICalTreeS
update key param = return $ H.insertWith (++) key [param]

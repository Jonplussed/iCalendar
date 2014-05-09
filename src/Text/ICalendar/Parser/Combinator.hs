module Text.ICalendar.Parser.Combinator
( ICalData (..)
, ICalTree
, iCalendar
) where

import qualified Data.Map as M
import Data.Monoid
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Pos

import Text.ICalendar.Type

type ICalTree = M.Map String [ICalData]
type ICalTreeS = ICalTree -> ICalTree

iCalendar :: Parser ICalTree
iCalendar = do
    params <- component
    eof
    return $ params M.empty

-- private functions

component :: Parser ICalTreeS
component = do
    pos <- getPosition
    string "BEGIN:"
    key <- manyTill upper newLine
    fs <- manyTill (component <|> property) (string $ "END:" ++ key)
    newLine
    update key $ ICalComponent (foldr1 (.) fs M.empty) pos

newLine :: Parser String
newLine = string "\r\n"

property :: Parser ICalTreeS
property = do
    pos <- getPosition
    key <- manyTill upper $ char ':'
    value <- toICalDataType key
    newLine
    update key value

update :: String -> ICalData -> Parser ICalTreeS
update key param = return $ M.insertWith (++) key [param]

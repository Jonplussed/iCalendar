module Text.ICalendar.Parser.Combinators where

import            Data.Monoid
import qualified  Data.HashMap.Lazy       as H
import            Text.Parsec.String
import            Text.Parsec.Combinator
import            Text.Parsec.Char
import            Text.Parsec.Prim
import            Text.Parsec.Pos

data ICalParam = EmptyNode
               | Property [String] SourcePos
               | Component [ICalTree] SourcePos
               | NodeNameConflict SourcePos
               deriving (Eq, Show)

instance Monoid ICalParam where
  mempty = EmptyNode
  EmptyNode `mappend` x = x
  Property s1 p1 `mappend` Property s2 p2 = Property (s1 ++ s2) p2
  Component c1 p1 `mappend` Component c2 p2 = Component (c1 ++ c2) p2
  _ `mappend` (Property _ p2) = NodeNameConflict p2
  _ `mappend` (Component _ p2) = NodeNameConflict p2


type ICalTree = H.HashMap String ICalParam
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
    update key $ Component [foldr1 (.) fs H.empty] pos

newLine :: Parser String
newLine = string "\r\n"

property :: Parser ICalTreeS
property = do
    pos <- getPosition
    key <- manyTill upper $ char ':'
    --segments <- sepBy1 (many1 anyChar) (newLine >> space)
    segments <- manyTill anyChar newLine
    update key $ Property [segments] pos

update :: String -> ICalParam -> Parser ICalTreeS
update key param = return $ H.insertWith mappend key param

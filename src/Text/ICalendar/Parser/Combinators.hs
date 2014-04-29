module Text.ICalendar.Parser.Combinators where

import Data.Monoid
import Text.ParserCombinators.Parsec
import qualified Data.HashMap.Lazy as H

data ICalParam = EmptyNode
               | Property [String]
               | Component [ICalTree]
               | NodeNameConflict
               deriving (Eq, Show)

type ICalTree = H.HashMap String ICalParam
type ICalTreeS = ICalTree -> ICalTree

instance Monoid ICalParam where
  mempty = EmptyNode
  EmptyNode `mappend` x = x
  Property x `mappend` Property y = Property $ x ++ y
  Component x `mappend` Component y = Component $ x ++ y
  _ `mappend` _ = NodeNameConflict

iCalendar :: Parser ICalTree
iCalendar = do
    params <- component
    eof
    return $ params H.empty

-- private functions

component :: Parser ICalTreeS
component = do
    string "BEGIN:"
    key <- manyTill upper newLine
    fs <- manyTill (component <|> property) (string $ "END:" ++ key)
    newLine
    update key $ Component [foldr1 (.) fs H.empty]

newLine :: Parser String
newLine = string "\r\n"

property :: Parser ICalTreeS
property = do
    key <- manyTill upper $ char ':'
    --segments <- sepBy1 (many1 anyChar) (newLine >> space)
    segments <- manyTill anyChar newLine
    update key $ Property [segments]

update :: String -> ICalParam -> Parser ICalTreeS
update key param = return $ H.insertWith mappend key param

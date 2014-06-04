module Text.ICalendar.Parser.Validator
( optCompN
, reqCompN
, optProp1
, reqProp1
, optPropN
, reqPropN
, reqCoProp1
) where

-- haskell platform libraries
import Control.Monad.Identity
import Data.Char
import Text.Parsec.String
import Text.Parsec.Char

-- foreign libraries
import Text.Parsec.Permutation

-- native libraries
import Text.ICalendar.Parser.Combinator

type Validator a = PermParser String () Identity a

optCompN :: Parser a -> String -> Validator [a]
optCompN = manyPerm .: component

reqCompN :: Parser a -> String -> Validator [a]
reqCompN = many1Perm .: component

optProp1 :: Parser a -> String -> Validator (Maybe a)
optProp1 = optionMaybePerm .: property

reqProp1 :: Parser a -> String -> Validator a
reqProp1 = oncePerm .: property

optPropN :: Parser a -> String -> Validator [a]
optPropN = manyPerm .: property

reqPropN :: Parser a -> String -> Validator [a]
reqPropN = many1Perm .: property

reqCoProp1 :: (Parser a, String) -> (Parser a, String) -> Validator a
reqCoProp1 = oncePerm .: coProperty

-- private functions

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f $ g x y

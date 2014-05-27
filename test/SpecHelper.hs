module SpecHelper where

import Text.Parsec.Error

instance Eq ParseError where
  e1 == e2 = errorPos e1 == errorPos e2 && errorMessages e1 == errorMessages e2

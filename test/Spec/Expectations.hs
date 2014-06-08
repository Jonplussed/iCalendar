module Spec.Expectations
( TestParser
, shouldFail
, shouldParseTo
, shouldSucceed
) where

-- haskell platform libraries
import Text.Parsec.Error

-- foreign libraries
import Test.Hspec

type TestParser a = Either ParseError a

instance Eq ParseError where
  e1 == e2 = errorPos e1 == errorPos e2 &&
    errorMessages e1 == errorMessages e2

shouldFail :: (Show a) => TestParser a -> Expectation
shouldFail parsed = parsed `shouldSatisfy` either (const True) (const False)

shouldSucceed :: (Show a) => TestParser a -> Expectation
shouldSucceed parsed = parsed `shouldSatisfy` either (const False) (const True)

shouldParseTo :: (Show a, Eq a) => TestParser a -> a -> Expectation
x `shouldParseTo` y = x `shouldBe` Right y

module Text.ICalendar.Parser.CombinatorSpec
( main
, spec
) where

-- foreign libraries
import Test.Hspec

-- native libraries
import SpecHelper
import Text.ICalendar.Parser.Combinator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lineBreak" $ do

    it "parses and returns line-breaks" $ do
      parseWith lineBreak "\r\n" `shouldParseTo` "\r\n"

  describe "property" $ do
    let parseProperty = parseLineWith $ property stubParser "PROPERTY"

    it "parses a key and returns a value" $ do
      parseProperty "PROPERTY:value" `shouldParseTo` "value"

  describe "coProperty" $ do
    let parseOneOrTwo = parseLineWith $
          coProperty (stubParser, "ONE") (stubParser, "TWO")

    it "parses the first of two given keys and returns the value" $ do
      parseOneOrTwo "ONE:one" `shouldParseTo` "one"

    it "parses the second of two given keys and returns the value" $ do
      parseOneOrTwo "TWO:two" `shouldParseTo` "two"

  describe "component" $ do
    let parseComponent = parseLinesWith $ component stubParser "COMPONENT"
        simpleComponent = [ "BEGIN:COMPONENT", "value", "END:COMPONENT" ]

    it "parses anything between begin/end component lines" $ do
      parseComponent simpleComponent `shouldParseTo` "value"

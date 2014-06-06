module Text.ICalendar.Component.VCalendarSpec
( main
, spec
) where

-- haskell platform libraries
import Data.List

-- foreign libraries
import Test.Hspec

-- native libraries
import SpecHelper
import Text.ICalendar.Component.VCalendar

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let prodidLine     = "PRODID:prodid"
      versionLine    = "VERSION:version"
      vCalendarLines = [ prodidLine, versionLine ]

      appendOnce line  = vCalendarLines ++ [line]
      appendTwice line = vCalendarLines ++ [line, line]
      parseVCalendar   = parseLinesWith vCalendar

  describe "test factory" $ do

    it "should generate a valid VCalendar" $ do
      shouldSucceed $ parseVCalendar vCalendarLines

  describe "vCalendar format" $ do

    it "must contain the properties first, then any components" $ do
      pendingWith "how to easily add a valid vEvent to the default lines"

  describe "vCalendar properties" $ do

    describe "PRODID" $ do

      it "requires a product ID" $ do
        shouldFail . parseVCalendar $ delete prodidLine vCalendarLines

      it "cannot have multiple product IDs" $ do
        shouldFail . parseVCalendar $ appendTwice prodidLine

      it "sets the \"productId\" record field" $ do
        let (Right newVCalendar) = parseVCalendar vCalendarLines
        productId newVCalendar `shouldBe` "prodid"

    describe "VERSION" $ do

      it "requries a version number" $ do
        shouldFail . parseVCalendar $ delete versionLine vCalendarLines

      it "cannot have multiple version numbers" $ do
        shouldFail . parseVCalendar $ appendTwice versionLine

      it "sets the \"version\" record field" $ do
        let (Right newVCalendar) = parseVCalendar vCalendarLines
        version newVCalendar `shouldBe` "version"

    describe "CALSCALE" $ do
      let calscaleLine = "CALSCALE:calscale"

      it "can have a single calendar scale" $ do
        shouldSucceed . parseVCalendar $ appendOnce calscaleLine

      it "cannot have multiple calendar scales" $ do
        shouldFail . parseVCalendar $ appendTwice calscaleLine

      it "sets the \"scale\" record field" $ do
        let (Right newVCalendar) = parseVCalendar $ appendOnce calscaleLine
        scale newVCalendar `shouldBe` (Just $ Unsupported "calscale")

    describe "METHOD" $ do
      let methodLine = "METHOD:method"

      it "can have a single method" $ do
        shouldSucceed . parseVCalendar $ appendOnce methodLine

      it "cannot have multiple methods" $ do
        shouldFail . parseVCalendar $ appendTwice methodLine

      it "sets the \"method\" record field" $ do
        let (Right newVCalendar) = parseVCalendar $ appendOnce methodLine
        method newVCalendar `shouldBe` Just "method"

  describe "vCalendar components" $ do

    it "can have multiple vEvents" $ do
      pendingWith "how to easily add a valid vEvent to the default lines"

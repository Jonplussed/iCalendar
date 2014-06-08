module Text.ICalendar.Component.VCalendarSpec
( main
, spec
) where

-- haskell platform libraries
import Data.List

-- foreign libraries
import Test.Hspec

-- native libraries
import Spec.Expectations
import Spec.Helpers
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
      parse            = parseLinesWith parseVCalendar

  describe "test factory" $ do

    it "should generate a valid VCalendar" $ do
      shouldSucceed $ parse vCalendarLines

  describe "vCalendar format" $ do

    it "must contain the properties first, then any components" $ do
      pendingWith "how to easily add a valid vEvent to the default lines"

  describe "vCalendar properties" $ do

    describe "PRODID" $ do

      it "requires a product ID" $ do
        shouldFail . parse $ delete prodidLine vCalendarLines

      it "cannot have multiple product IDs" $ do
        shouldFail . parse $ appendTwice prodidLine

      it "sets the \"productId\" record field" $ do
        let (Right newVCalendar) = parse vCalendarLines
        productId newVCalendar `shouldBe` "prodid"

    describe "VERSION" $ do

      it "requries a version number" $ do
        shouldFail . parse $ delete versionLine vCalendarLines

      it "cannot have multiple version numbers" $ do
        shouldFail . parse $ appendTwice versionLine

      it "sets the \"version\" record field" $ do
        let (Right newVCalendar) = parse vCalendarLines
        version newVCalendar `shouldBe` "version"

    describe "CALSCALE" $ do
      let calscaleLine = "CALSCALE:calscale"

      it "can have a single calendar scale" $ do
        shouldSucceed . parse $ appendOnce calscaleLine

      it "cannot have multiple calendar scales" $ do
        shouldFail . parse $ appendTwice calscaleLine

      context "with an unrecognized calendar scale" $ do
        let (Right newVCalendar) = parse $ appendOnce calscaleLine

        it "sets the \"scale\" record field to unsupported" $ do
          scale newVCalendar `shouldBe` (Unsupported "calscale")

      context "with a Gregorian calendar scale" $ do
        let (Right newVCalendar) = parse $ appendOnce "CALSCALE:GREGORIAN"

        it "sets the \"scale\" record field to Gregorian" $ do
          scale newVCalendar `shouldBe` Gregorian

    describe "METHOD" $ do
      let methodLine = "METHOD:method"

      it "can have a single method" $ do
        shouldSucceed . parse $ appendOnce methodLine

      it "cannot have multiple methods" $ do
        shouldFail . parse $ appendTwice methodLine

      it "sets the \"method\" record field" $ do
        let (Right newVCalendar) = parse $ appendOnce methodLine
        method newVCalendar `shouldBe` Just "method"

  describe "vCalendar components" $ do

    it "can have multiple vEvents" $ do
      pendingWith "how to easily add a valid vEvent to the default lines"

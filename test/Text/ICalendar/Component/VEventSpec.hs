module Text.ICalendar.Component.VEventSpec
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
import Text.ICalendar.Component.VEvent

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let dtStartLine  = "DTSTART:dtstart"
      dtEndLine = "DTEND:dtend"
      vEventLines = [ dtStartLine, dtEndLine ]

      appendOnce line  = vEventLines ++ [line]
      appendTwice line = vEventLines ++ [line, line]
      parse            = parseLinesWith parseVEvent

  describe "test factory" $ do

    it "should generate a valid VEvent" $ do
      shouldSucceed $ parse vEventLines

  describe "vEvent properties" $ do

    describe "DTSTART" $ do

      it "requires a start date" $ do
        shouldFail . parse $ delete dtStartLine vEventLines

      it "cannot have multiple start dates" $ do
        shouldFail . parse $ appendTwice dtStartLine

      it "sets the \"startDate\" record field" $ do
        let (Right newVEvent) = parse vEventLines
        startDate newVEvent `shouldBe` "dtstart"

    describe "ATTENDEE" $ do
      let attendeeLine = "ATTENDEE:attendee"

      it "can have multiple attendees" $ do
        shouldSucceed . parse $ appendTwice attendeeLine

      it "sets the attendees' values to the \"attendees\" record field" $ do
        let (Right newVEvent) = parse $ appendTwice attendeeLine
        attendees newVEvent `shouldBe` ["attendee","attendee"]

    describe "ORGANIZER" $ do
      let organizerLine = "ORGANIZER:organizer"

      it "can have an organizer" $ do
        shouldSucceed . parse $ appendOnce organizerLine

      it "cannot have multiple organizers" $ do
        shouldFail . parse $ appendTwice organizerLine

      it "sets the \"organizer\" record field" $ do
        let (Right newVEvent) = parse $ appendOnce organizerLine
        organizer newVEvent `shouldBe` Just "organizer"

    describe "LOCATION" $ do
      let locationLine = "LOCATION:location"

      it "can have an location" $ do
        shouldSucceed . parse $ appendOnce locationLine

      it "cannot have multiple locations" $ do
        shouldFail . parse $ appendTwice locationLine

      it "sets the \"location\" record field" $ do
        let (Right newVEvent) = parse $ appendOnce locationLine
        location newVEvent `shouldBe` Just "location"

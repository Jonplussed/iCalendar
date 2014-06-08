module Text.ICalendar.DataType.DurationSpec
( main
, spec
) where

-- haskell platform libraries
import Data.Time.Clock (DiffTime, secondsToDiffTime)

-- foreign libraries
import Test.Hspec

-- native libraries
import Spec.Expectations
import Spec.Helpers
import Text.ICalendar.DataType.Duration

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  let parse = parseLineWith durationType

  describe "asDuration" $ do

    describe "parsing a date-formatted duration" $ do

      it "parses days" $ do
        parse "P1DT" `shouldParseTo` secondsToDiffTime 86400

      it "parses hours" $ do
        parse "P0DT1H" `shouldParseTo` secondsToDiffTime 3600

      it "parses minutes" $ do
        parse "P0DT1M" `shouldParseTo` secondsToDiffTime 60

      it "parses seconds" $ do
        parse "P0DT1S" `shouldParseTo` secondsToDiffTime 1

      it "parses days, hours, minutes, and seconds" $ do
        parse "P0DT0H0M1S" `shouldParseTo` secondsToDiffTime (1)

      it "parses a negative date" $ do
        parse "-P0DT1S" `shouldParseTo` secondsToDiffTime (-1)

      it "should not parse units out of order" $ do
        shouldFail $ parse "P0DT0S0H"

    describe "parsing a time-formatted duration" $ do

      it "parses hours" $ do
        parse "PT1H" `shouldParseTo` secondsToDiffTime 3600

      it "parses minutes" $ do
        parse "PT1M" `shouldParseTo` secondsToDiffTime 60

      it "parses seconds" $ do
        parse "PT1S" `shouldParseTo` secondsToDiffTime 1

      it "parses hours, minutes, and seconds" $ do
        parse "PT0H0M1S" `shouldParseTo` secondsToDiffTime (1)

      it "parses a negative time" $ do
        parse "-PT1S" `shouldParseTo` secondsToDiffTime (-1)

      it "should not parse units out of order" $ do
        shouldFail $ parse "PT0S0H"

    describe "parsing a week-formatted duration" $ do

      it "parses weeks" $ do
        parse "P1W" `shouldParseTo` secondsToDiffTime 604800

      it "can be negative" $ do
        parse "-P1W" `shouldParseTo` secondsToDiffTime (-604800)

module Text.ICalendar.DataType.DurationSpec
( main
, spec
) where

-- haskell platform libraries
import Data.Time.Clock (DiffTime, secondsToDiffTime)

-- foreign libraries
import Test.Hspec

-- native libraries
import SpecHelper
import Text.ICalendar.DataType.Duration

main :: IO ()
main = hspec spec

parse :: String -> TestParser DiffTime
parse = parseLineWith asDuration

spec :: Spec
spec = do
  describe "asDuration" $ do

    describe "parsing a date-formatted duration" $ do

      it "parses days" $ do
        parse "P1DT0H0M0S" `shouldParseTo` secondsToDiffTime 86400

      it "parses hours" $ do
        parse "P0DT1H0M0S" `shouldParseTo` secondsToDiffTime 3600

      it "parses minutes" $ do
        parse "P0DT0H1M0S" `shouldParseTo` secondsToDiffTime 60

      it "parses seconds" $ do
        parse "P0DT0H0M1S" `shouldParseTo` secondsToDiffTime 1

      it "can be negative" $ do
        parse "-P0DT0H0M1S" `shouldParseTo` secondsToDiffTime (-1)

    describe "parsing a time-formatted duration" $ do

      it "parses hours" $ do
        parse "PT1H0M0S" `shouldParseTo` secondsToDiffTime 3600

      it "parses minutes" $ do
        parse "PT0H1M0S" `shouldParseTo` secondsToDiffTime 60

      it "parses seconds" $ do
        parse "PT0H0M1S" `shouldParseTo` secondsToDiffTime 1

      it "can be negative" $ do
        parse "-PT0H0M1S" `shouldParseTo` secondsToDiffTime (-1)

    describe "parsing a week-formatted duration" $ do

      it "parses weeks" $ do
        parse "P1W" `shouldParseTo` secondsToDiffTime 604800

      it "can be negative" $ do
        parse "-P1W" `shouldParseTo` secondsToDiffTime (-604800)

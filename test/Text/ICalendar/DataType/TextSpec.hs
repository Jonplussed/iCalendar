module Text.ICalendar.DataType.TextSpec
( main
, spec
) where

-- haskell platform libraries
import qualified Text.Parsec.Prim as P
import Text.Parsec.Error (ParseError)

-- foreign libraries
import Test.Hspec

-- native libraries
import SpecHelper
import Text.ICalendar.DataType.Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "asText" $ do
    let line1 = "first line"
        line2 = "second line"

    describe "parsing a single line of content" $ do

      it "parses the line of text" $ do
        parse line1 `shouldBe` Right line1

    describe "parsing multiple lines of content" $ do
      let parsed = Right $ line1 ++ " " ++ line2

      context "split via newline followed by a space" $ do

        it "parses the lines and joins them with a space" $ do
          let lines = line1 ++ "\r\n " ++ line2
          parse lines `shouldBe` parsed

      context "split via newline followed by a tab" $ do

        it "parses the lines and joins them with a space" $ do
          let lines = line1 ++ "\r\n\t" ++ line2
          parse lines `shouldBe` parsed

-- private functions

parse :: String -> Either ParseError String
parse str = P.parse asText "text datatype" $ str ++ newLine

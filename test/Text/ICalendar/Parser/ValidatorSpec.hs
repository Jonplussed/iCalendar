module Text.ICalendar.Parser.ValidatorSpec
( main
, spec
) where

-- haskell platform libraries
import Text.Parsec.Prim

-- foreign libraries
import Test.Hspec
import Text.Parsec.Permutation

-- native libraries
import SpecHelper
import Text.ICalendar.Parser.Validator

main :: IO ()
main = hspec spec

validateWith validator key =
  parseLinesWith . runPermParser $ validator stubParser key

coValidateWith validator key1 key2 =
  parseLinesWith . runPermParser $ validator (stubParser, key1) (stubParser, key2)

spec :: Spec
spec = do
  let component = [ "BEGIN:COMPONENT"
                  , "value1"
                  , "END:COMPONENT"
                  ]

      components = component ++ [ "BEGIN:COMPONENT"
                                , "value2"
                                , "END:COMPONENT"
                                ]

      property = ["PROPERTY:value1"]
      properties = property ++ ["PROPERTY:value2"]

  describe "optCompN" $ do

    it "gets the values of all of the components" $ do
      validateWith optCompN "COMPONENT" components `shouldParseTo` ["value1", "value2"]

    it "succeeds if zero of the component appear" $ do
      validateWith optCompN "NOTHING" components `shouldParseTo` []

  describe "reqCompN" $ do

    it "gets the values of all of the components" $ do
      validateWith reqCompN "COMPONENT" components `shouldParseTo` ["value1", "value2"]

    it "errors if none of the given component appear" $ do
      shouldFail $ validateWith reqCompN "NOTHING" components

  describe "optProp1" $ do

    it "gets the value of the given property" $ do
      validateWith optProp1 "PROPERTY" property `shouldParseTo` Just "value1"

    it "errors if more than one of the property appear" $ do
      shouldFail $ validateWith optProp1 "PROPERTY" properties

    it "succeeds if zero of the property appear" $ do
      validateWith optProp1 "NOTHING" properties `shouldParseTo` Nothing

  describe "reqProp1" $ do

    it "gets the value of the given property" $ do
      validateWith reqProp1 "PROPERTY" property `shouldParseTo` "value1"

    it "errors if more than one of the property appear" $ do
      shouldFail $ validateWith reqProp1 "PROPERTY" properties

    it "errors if zero of the property appear" $ do
      shouldFail $ validateWith reqProp1 "NOTHING" properties

  describe "optPropN" $ do

    it "gets the values of the given properties" $ do
      validateWith optPropN "PROPERTY" properties `shouldParseTo`
        ["value1", "value2"]

    it "succeeds if zero of the property appear" $ do
      validateWith optPropN "NOTHING" properties `shouldParseTo` []

  describe "reqPropN" $ do

    it "gets the values of the given properties" $ do
      validateWith reqPropN "PROPERTY" properties `shouldParseTo`
        ["value1", "value2"]

    it "errors if zero of the property appear" $ do
      shouldFail $ validateWith reqPropN "NOTHING" properties

  describe "reqCoProp1" $ do
    let property1    = ["PROPERTY1:value1"]
        property2    = ["PROPERTY2:value2"]

    it "gets the value of the first desired property" $ do
      coValidateWith reqCoProp1 "PROPERTY1" "PROPERTY2" property1 `shouldParseTo`
        "value1"

    it "gets the value of the second desired property" $ do
      coValidateWith reqCoProp1 "PROPERTY1" "PROPERTY2" property2 `shouldParseTo`
        "value2"

    it "errors if the same desired property appears more than once" $ do
      shouldFail $ coValidateWith reqCoProp1 "PROPERTY1" "PROPERTY2" (property1 ++ property1)

    it "errors if both desired properties appear" $ do
      shouldFail $ coValidateWith reqCoProp1 "PROPERTY1" "PROPERTY2" (property1 ++ property2)

    it "errors if neither desired properties appears" $ do
      shouldFail $ coValidateWith reqCoProp1 "NOTHING1" "NOTHING2" (property1 ++ property2)

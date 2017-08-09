import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck

import Data.Either (isLeft)

import RPN


main :: IO ()
main = hspecWith defaultConfig {configFastFail = False} evaluateSpecs

evaluateSpecs :: Spec
evaluateSpecs = describe "evaluate" $ do

  it "fails on empty input" $ do
    evaluate "" `shouldSatisfy` isLeft

  it "handles positive and negative numbers" $ do
    property $ \n -> evaluate (show n) == Right n

  it "can add" $ do
    evaluate "1 2 +" `shouldBe` Right 3

  it "can multiply" $ do
    evaluate "1 2 *" `shouldBe` Right 2

  it "handles a chain of operations" $ do
    evaluate "1 2 + 3 *" `shouldBe` Right 9
    evaluate "-1 2 -7 2 + * +" `shouldBe` Right (-11)

  it "fails on invalid input" $ do
    evaluate "3 2 #" `shouldSatisfy` isLeft
    evaluate "3 2+" `shouldSatisfy` isLeft
    evaluate "*" `shouldSatisfy` isLeft
    evaluate "1 2 * +" `shouldSatisfy` isLeft
    evaluate "100 34" `shouldSatisfy` isLeft
    evaluate "1 2 -3 +" `shouldSatisfy` isLeft

  -- TODO: Test for nice error messages by using the following tests
  -- or creating your own error datatype!
  {-
  it "gives nice error messages" $ do
    evaluate "3 2 #" `shouldBe` Left "invalid token: \"#\""
    evaluate "3 2+" `shouldBe` Left "invalid token: \"2+\""
    evaluate "*" `shouldBe` Left "not enough arguments for operator '*'"
    evaluate "1 2 * +" `shouldBe` Left "not enough arguments for operator \"+\""
    evaluate "100 34" `shouldBe` Left "more than one value left: [34, 100]"
    evaluate "1 2 -3 +" `shouldBe` Left "more than one value left: [-1, 1]"
  -}

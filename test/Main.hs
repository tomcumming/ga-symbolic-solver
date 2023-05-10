module Main (main) where

import Symbolic.Scalar qualified as Scalar
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain scalarTests

scalarTests :: TestTree
scalarTests =
  testGroup
    "Scalar Tests"
    [scalarShow]

data Var
  = A
  | B
  | C
  deriving (Show, Eq, Ord)

scalarShow :: TestTree
scalarShow =
  testGroup
    "Scalar Show"
    [ showTest 0 "0",
      showTest 1 "1",
      showTest 123 "123",
      showTest (Scalar.var A) "A",
      showTest (Scalar.var A * Scalar.var A) "A^2",
      showTest (Scalar.var A * Scalar.var B) "A * B",
      showTest (Scalar.var A * Scalar.var B + 1) "1 + A * B",
      showTest ((Scalar.var A + 1) * Scalar.var B) "A * B + B",
      showTest (Scalar.var A * 2) "2 * A"
    ]
  where
    showTest :: Scalar.Scalar Var -> String -> TestTree
    showTest s str = testCase str $ show s @?= str

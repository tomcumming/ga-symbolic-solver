module Main (main) where

import Symbolic.GA (GA, basis, calculate, revers, var)
import Symbolic.Scalar qualified as Scalar
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

main :: IO ()
main = defaultMain scalarTests

scalarTests :: TestTree
scalarTests =
  testGroup
    "Scalar Tests"
    [scalarShow, gaShow, gaReverse]

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

data Basis
  = E0
  | E1
  | E2
  deriving (Show, Eq, Ord)

squareBasis :: Basis -> Rational
squareBasis = \case
  E0 -> 0
  E1 -> 1
  E2 -> -1

gaShow :: TestTree
gaShow =
  testGroup
    "GA Show"
    [ showTest 0 "0",
      showTest 1 "1",
      showTest (basis E0) "E0",
      showTest (basis E0 * basis E1) "E0 * E1",
      showTest (basis E1 * basis E0) "-1 * E0 * E1",
      showTest ((basis E1 + 1) * basis E2) "E1 * E2 + E2"
    ]
  where
    showTest :: GA Basis Var -> String -> TestTree
    showTest ga str = testCase str $ show (calculate squareBasis ga) @?= str

gaReverse :: TestTree
gaReverse =
  testGroup
    "GA Reverse"
    [ testIdentity (123 * var A),
      testIdentity (basis E0),
      testIdentity (basis E2),
      testChanged (basis E0 * basis E1) (basis E1 * basis E0),
      testChanged (basis E1 * basis E0) (basis E0 * basis E1),
      testChanged (basis E1 * basis E2) (basis E2 * basis E1),
      testChanged
        (basis E0 * basis E1 * basis E2)
        (basis E2 * basis E1 * basis E0)
    ]
  where
    testIdentity :: GA Basis Var -> TestTree
    testIdentity ga =
      let mv = calculate squareBasis ga
          mv' = calculate squareBasis (revers ga)
       in testCase ("Identity " <> show mv) $ mv' @?= mv

    testChanged :: GA Basis Var -> GA Basis Var -> TestTree
    testChanged ga ga' =
      let original = calculate squareBasis ga
          mv = calculate squareBasis (revers ga)
          mv' = calculate squareBasis ga'
       in testCase ("Reverse " <> show mv') $ do
            assertBool "Examples are different" $ original /= mv
            mv' @?= mv

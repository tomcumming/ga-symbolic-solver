module Main (main) where

import Data.Set qualified as S
import Symbolic.GA (Basis (..), GA, MV, calculate, dual, revers, undual, var, vec)
import Symbolic.Scalar qualified as Scalar
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

main :: IO ()
main = defaultMain scalarTests

scalarTests :: TestTree
scalarTests =
  testGroup
    "Scalar Tests"
    [scalarShow, gaShow, gaReverse, gaDualUndual, gaDualExamples]

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

data BasisVec
  = E0
  | E1
  | E2
  | E3
  deriving (Show, Eq, Ord)

basis :: Basis BasisVec
basis =
  Basis
    { basSquare = \case
        E0 -> 0
        E1 -> 1
        E2 -> 1
        E3 -> 1,
      basPss = S.fromList [E0, E1, E2, E3]
    }

gaShow :: TestTree
gaShow =
  testGroup
    "GA Show"
    [ showTest 0 "0",
      showTest 1 "1",
      showTest (vec E0) "E0",
      showTest (vec E0 * vec E1) "E0 * E1",
      showTest (vec E1 * vec E0) "-1 * E0 * E1",
      showTest ((vec E1 + 1) * vec E2) "E1 * E2 + E2"
    ]
  where
    showTest :: GA BasisVec Var -> String -> TestTree
    showTest ga str = testCase str $ show (calculate basis ga) @?= str

gaReverse :: TestTree
gaReverse =
  testGroup
    "GA Reverse"
    [ testIdentity (123 * var A),
      testIdentity (vec E0),
      testIdentity (vec E2),
      testChanged (vec E0 * vec E1) (vec E1 * vec E0),
      testChanged (vec E1 * vec E0) (vec E0 * vec E1),
      testChanged (vec E1 * vec E2) (vec E2 * vec E1),
      testChanged
        (vec E0 * vec E1 * vec E2)
        (vec E2 * vec E1 * vec E0)
    ]
  where
    testIdentity :: GA BasisVec Var -> TestTree
    testIdentity ga =
      let mv = calculate basis ga
          mv' = calculate basis (revers ga)
       in testCase ("Identity " <> show mv) $ mv' @?= mv

    testChanged :: GA BasisVec Var -> GA BasisVec Var -> TestTree
    testChanged ga ga' =
      let original = calculate basis ga
          mv = calculate basis (revers ga)
          mv' = calculate basis ga'
       in testCase ("Reverse " <> show mv') $ do
            assertBool "Examples are different" $ original /= mv
            mv' @?= mv

gaDualUndual :: TestTree
gaDualUndual =
  testGroup "UnDual . Dual"
    $ map
      ( \ga ->
          let mv :: MV BasisVec Var = calculate basis ga
              mv' = calculate basis (undual (dual ga))
           in testCase (show mv) $ mv' @?= mv
      )
      . fmap (product . fmap vec . S.toList)
    $ S.toList (S.powerSet (basPss basis))

-- | Examples takes from PGA4CS
gaDualExamples :: TestTree
gaDualExamples =
  testGroup "Dual Examples" $
    map
      ( \(ga, ga') ->
          let original = calculate basis ga
              mv :: MV BasisVec Var = calculate basis (dual ga)
              mv' = calculate basis ga'
           in testCase ("Dual " <> show original) $ mv @?= mv'
      )
      [ (1, vec E0 * vec E1 * vec E2 * vec E3),
        (vec E0, vec E1 * vec E2 * vec E3),
        (vec E1, vec E0 * vec E3 * vec E2),
        (vec E2, vec E0 * vec E1 * vec E3),
        (vec E3, vec E0 * vec E2 * vec E1),
        (vec E0 * vec E1, vec E2 * vec E3),
        (vec E0 * vec E2, vec E3 * vec E1),
        (vec E0 * vec E3, vec E1 * vec E2),
        (vec E2 * vec E3, vec E0 * vec E1),
        (vec E3 * vec E1, vec E0 * vec E2),
        (vec E1 * vec E2, vec E0 * vec E3),
        (vec E1 * vec E2 * vec E3, -1 * vec E0),
        (vec E0 * vec E3 * vec E2, -1 * vec E1),
        (vec E0 * vec E1 * vec E3, -1 * vec E2),
        (vec E0 * vec E2 * vec E1, -1 * vec E3),
        (vec E0 * vec E1 * vec E2 * vec E3, 1)
      ]

-- Covers Pawl.Extra.Int, Pawl.Extra.Integer, and Pawl.Extra.Natural.
module Pawl.ExtraSpec where

import qualified Data.Sequence as Seq
import qualified Numeric.Natural
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Extra.Natural as Natural
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

-- The first Natural that does not fit in a 64-bit Int, written as a literal so
-- the boundary cases below cannot inherit the conversion they are checking.
-- The "largest Int" cases pin the width: they fail on a machine where Int is
-- not 64 bits, rather than passing vacuously.
tooBig :: Numeric.Natural.Natural
tooBig = 2 ^ (63 :: Int)

-- The smallest and largest Int, as Integers, for the cases that step off each
-- end. toInteger is total in both directions and is not one of the restricted
-- conversions.
biggestInt :: Integer
biggestInt = toInteger (maxBound :: Int)

smallestInt :: Integer
smallestInt = toInteger (minBound :: Int)

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.ExtraSpec"
    [ Tasty.testGroup
        "Natural.toInt"
        [ HU.testCase "zero" $
            HU.assertEqual "0" (Just 0) (Natural.toInt 0),
          HU.testCase "the largest Int" $
            HU.assertEqual "maxBound" (Just maxBound) (Natural.toInt (tooBig - 1)),
          HU.testCase "one past the largest Int" $
            HU.assertEqual "no wrap" Nothing (Natural.toInt tooBig),
          HU.testCase "far past the largest Int" $
            HU.assertEqual "no wrap" Nothing (Natural.toInt (tooBig * tooBig)),
          QC.testProperty "never lies about the value it returns" $
            \i ->
              let n = Int.toNaturalSaturating i
               in QC.property (maybe True (\m -> toInteger m == toInteger n) (Natural.toInt n))
        ],
      Tasty.testGroup
        "Natural.toIntSaturating"
        [ HU.testCase "zero" $
            HU.assertEqual "0" 0 (Natural.toIntSaturating 0),
          HU.testCase "the largest Int" $
            HU.assertEqual "maxBound" maxBound (Natural.toIntSaturating (tooBig - 1)),
          HU.testCase "one past the largest Int stops at maxBound" $
            HU.assertEqual "maxBound" maxBound (Natural.toIntSaturating tooBig),
          -- The saturation is what makes this safe as a take/drop argument: a
          -- count no list can reach behaves exactly like maxBound.
          HU.testCase "an unreachable count still takes the whole list" $
            HU.assertEqual "abc" "abc" (take (Natural.toIntSaturating tooBig) "abc"),
          QC.testProperty "agrees with toInt wherever toInt is defined" $
            \i ->
              let n = Int.toNaturalSaturating i
               in QC.property (maybe (Natural.toIntSaturating n == maxBound) (== Natural.toIntSaturating n) (Natural.toInt n))
        ],
      Tasty.testGroup
        "Natural.length"
        [ HU.testCase "the empty list" $
            HU.assertEqual "0" 0 (Natural.length ([] :: [Char])),
          HU.testCase "a sequence, not just a list" $
            HU.assertEqual "3" 3 (Natural.length (Seq.fromList "abc")),
          QC.testProperty "agrees with the Foldable length" $
            \xs -> toInteger (Natural.length (xs :: [Int])) QC.=== toInteger (length xs)
        ],
      Tasty.testGroup
        "Int.toNatural"
        [ HU.testCase "zero" $
            HU.assertEqual "0" (Just 0) (Int.toNatural 0),
          HU.testCase "the largest Int" $
            HU.assertEqual "maxBound" (Just (tooBig - 1)) (Int.toNatural maxBound),
          HU.testCase "a negative Int has no Natural" $
            HU.assertEqual "no wrap" Nothing (Int.toNatural (-1)),
          HU.testCase "the smallest Int has no Natural" $
            HU.assertEqual "no wrap" Nothing (Int.toNatural minBound)
        ],
      Tasty.testGroup
        "Int.toNaturalSaturating"
        [ HU.testCase "a negative Int stops at zero" $
            HU.assertEqual "0" 0 (Int.toNaturalSaturating (-1)),
          HU.testCase "the smallest Int stops at zero" $
            HU.assertEqual "0" 0 (Int.toNaturalSaturating minBound),
          QC.testProperty "is the value clamped below at zero" $
            \i -> toInteger (Int.toNaturalSaturating i) QC.=== max 0 (toInteger i)
        ],
      Tasty.testGroup
        "Integer.toNatural"
        [ HU.testCase "zero" $
            HU.assertEqual "0" (Just 0) (Integer.toNatural 0),
          HU.testCase "a value beyond Int is still fine" $
            HU.assertEqual "big" (Just (tooBig * tooBig)) (Integer.toNatural (toInteger tooBig * toInteger tooBig)),
          HU.testCase "a negative Integer has no Natural" $
            HU.assertEqual "no wrap" Nothing (Integer.toNatural (-1)),
          QC.testProperty "never lies about the value it returns" $
            \i -> QC.property (maybe (i < 0) (\n -> toInteger n == i) (Integer.toNatural i))
        ],
      Tasty.testGroup
        "Integer.toNaturalSaturating"
        [ -- CR 107.1b: "If a calculation that would determine the result of an
          -- effect yields a negative number, zero is used instead." The floor is
          -- the rule, which is why every count-shaped caller wants it.
          HU.testCase "CR 107.1b: a negative Integer stops at zero" $
            HU.assertEqual "0" 0 (Integer.toNaturalSaturating (-7)),
          -- The crash this replaces: fromInteger on a negative value bound for
          -- a Natural throws, and the rules do produce negative quantities.
          HU.testCase "a value beyond Int is still exact" $
            HU.assertEqual "big" (tooBig * tooBig) (Integer.toNaturalSaturating (toInteger tooBig * toInteger tooBig)),
          QC.testProperty "is the value clamped below at zero" $
            \i -> toInteger (Integer.toNaturalSaturating i) QC.=== max 0 i
        ],
      Tasty.testGroup
        "Integer.toInt"
        [ HU.testCase "zero" $
            HU.assertEqual "0" (Just 0) (Integer.toInt 0),
          HU.testCase "the largest Int" $
            HU.assertEqual "maxBound" (Just maxBound) (Integer.toInt biggestInt),
          HU.testCase "one past the largest Int" $
            HU.assertEqual "no wrap" Nothing (Integer.toInt (biggestInt + 1)),
          HU.testCase "one below the smallest Int" $
            HU.assertEqual "no wrap" Nothing (Integer.toInt (smallestInt - 1))
        ],
      Tasty.testGroup
        "Integer.toIntSaturating"
        [ HU.testCase "a value that fits is unchanged" $
            HU.assertEqual "7" 7 (Integer.toIntSaturating 7),
          HU.testCase "too big stops at maxBound" $
            HU.assertEqual "maxBound" maxBound (Integer.toIntSaturating (biggestInt + 1)),
          HU.testCase "too small stops at minBound" $
            HU.assertEqual "minBound" minBound (Integer.toIntSaturating (smallestInt - 1)),
          -- Both saturations are what make this safe as a take/drop argument:
          -- an unreachable count behaves like maxBound, a negative one like none.
          HU.testCase "an unreachable count still takes the whole list" $
            HU.assertEqual "abc" "abc" (take (Integer.toIntSaturating (biggestInt + 1)) "abc"),
          HU.testCase "a negative count takes nothing" $
            HU.assertEqual "empty" "" (take (Integer.toIntSaturating (-1)) "abc"),
          QC.testProperty "never crosses a bound it did not start on" $
            \i -> toInteger (Integer.toIntSaturating i) QC.=== max smallestInt (min biggestInt i)
        ]
    ]

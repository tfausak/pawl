module Pawl.JsonSchema.PatternSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.JsonSchema.Pattern as Pattern
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonSchema.Pattern" $ do
  let matches label source value expected =
        Spec.it s label $
          Spec.assertEq s (Pattern.matches (Text.pack source) (Text.pack value)) (Right expected)
      unsupported label source =
        Spec.it s (label <> " is unsupported") $
          Spec.assertBool
            s
            (Either.isLeft (Pattern.matches (Text.pack source) (Text.pack "a")))
            "expected the pattern to be unsupported"

  matches "a literal matches itself" "^ab$" "ab" True
  -- Whole-string, so a pattern matching a prefix does not match the string.
  matches "a literal does not match a longer string" "^ab$" "abc" False
  matches "the empty pattern matches only the empty string" "^$" "" True

  matches "a class matches a member" "^[a-c]$" "b" True
  matches "a class does not match a non-member" "^[a-c]$" "d" False
  matches "a class matches a bare member beside a range" "^[a-cx]$" "x" True

  matches "a star matches nothing" "^ab*$" "a" True
  matches "a star matches repeats" "^ab*$" "abbb" True
  matches "a star does not match another character" "^ab*$" "ac" False

  matches "an alternation matches its first branch" "^a|b$" "a" True
  matches "an alternation matches its last branch" "^a|b$" "b" True
  matches "an alternation matches neither" "^a|b$" "c" False

  -- The pattern Pawl.JsonCodec.Common.naturalMap publishes, which is the whole
  -- reason this module exists: a group, an alternation inside it, a class and a
  -- starred class.
  let natural = "^(0|[1-9][0-9]*)$"
  matches "a natural key of one digit" natural "0" True
  matches "a natural key of several digits" natural "100" True
  matches "a natural key with a leading zero" natural "01" False
  matches "a natural key with a sign" natural "-1" False
  matches "a natural key with an exponent" natural "1e0" False
  matches "a natural key that is not a number at all" natural "abc" False

  -- A group that can match the empty string, so the repetition has to notice it
  -- made no progress rather than looping. A failure here is the suite's timeout
  -- rather than an assertion.
  matches "a star over a group that matches nothing terminates" "^(a*)*b$" "b" True

  unsupported "an unanchored pattern" "a"
  unsupported "a half-anchored pattern" "^a"
  unsupported "an escape" "^\\d$"
  unsupported "a negated class" "^[^a]$"
  unsupported "plus" "^a+$"
  unsupported "the wildcard" "^.$"
  unsupported "an unclosed group" "^(a$"

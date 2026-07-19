{-# LANGUAGE DeriveLift #-}

module Pawl.Type.Keyword where

import Language.Haskell.TH.Syntax (Lift)

-- CR 702. A keyword is a CITATION, not an effect: rule 702 is part of the
-- comprehensive rules, the same as rule 506 or rule 302.
--
-- Casing on this is NOT a violation of the closed/open invariant. That invariant
-- forbids the rules core casing on the IDENTITY OF AN EFFECT; a keyword is a
-- numbered rule, so `case keyword of Flying -> ...` is the same kind of act as
-- casing on Phase. The test is "is it in the rulebook?" -- Flying is 702.9;
-- Goblin Piker is not in the rulebook. See the M2a spec, section 1, before
-- "fixing" this into a classification.
--
-- Constructors are ordered by RULE NUMBER, not by arrival, so this type stays
-- diffable against rule 702 itself. Nine, because nine have consumers -- M2b
-- added FirstStrike (702.7) and DoubleStrike (702.4); M2c inserts Deathtouch
-- (702.2) and Trample (702.19).
--
-- Grows a parameterized constructor at the punchlist: Landwalk Subtype (702.14),
-- and later Protection Quality (702.16) and Ward Cost (702.21). A `data`, not an
-- enum, so that is an addition rather than a reshape.
data Keyword
  = Deathtouch -- 702.2
  | Defender -- 702.3
  | DoubleStrike -- 702.4
  | FirstStrike -- 702.7
  | Flying -- 702.9
  | Haste -- 702.10
  | Reach -- 702.17
  | Trample -- 702.19
  | Vigilance -- 702.20
  deriving (Eq, Lift, Ord, Show)

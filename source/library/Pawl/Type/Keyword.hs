module Pawl.Type.Keyword where

import Numeric.Natural (Natural)

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
-- diffable against rule 702 itself. Each constructor below has a consumer --
-- P3a adds Fear (702.36, a colour-and-artifact blocking restriction) and
-- Devoid (702.114, a characteristic-defining ability that makes an object
-- colourless); P10 adds Infect (702.90, a deal-time damage-diversion bit).
--
-- Toxic (702.164) is the first PARAMETERIZED constructor, exactly the addition
-- the `data`-not-an-enum choice was made for; Landwalk Subtype (702.14),
-- Protection Quality (702.16) and Ward Cost (702.21) follow the same shape.
--
-- Multiplicity is NOT this type's problem: an object can have the same keyword
-- ability twice, which Pawl.Type.ProjectedCharacteristics.keywords carries as a
-- count. This type says only WHICH ability, so a card's printed keywords stay a
-- Set (each printed once) -- see Pawl.Type.Card.keywords.
data Keyword
  = Deathtouch -- 702.2
  | Defender -- 702.3
  | DoubleStrike -- 702.4
  | FirstStrike -- 702.7
  | Flying -- 702.9
  | Haste -- 702.10
  | Indestructible -- 702.12
  | Reach -- 702.17
  | Trample -- 702.19
  | Vigilance -- 702.20
  | Fear -- 702.36
  | Infect -- 702.90
  | Devoid -- 702.114
  | -- 702.164a: "Toxic is a static ability. It is written 'toxic N,' where N is
    -- a number." N rides the constructor, so `Toxic 1` and `Toxic 2` are
    -- distinct keywords, and CR 702.164b's total toxic value is the sum over
    -- every toxic ability the creature has (Pawl.Projection.totalToxic) --
    -- including two with the same N, which the projection counts separately.
    Toxic Natural
  deriving (Eq, Ord, Show)

-- Devoid is read only at the projection SEED (Projection.baseColorsOf), not as a
-- layer-6 GainKeyword grant -- so a Devoid GRANTED by a layer-6 effect does
-- nothing to colour, silently. Expressible open-half data today; no card in the
-- pool does it (#35). See Projection.baseColorsOf's comment for the full
-- argument.

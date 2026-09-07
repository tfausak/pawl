module Pawl.Types.Prototype where

import qualified Pawl.Types.ManaCost as ManaCost

-- | The payload of Pawl.Types.Keyword's Prototype arm: the second set of mana
-- cost, power and toughness characteristics CR 718.1 prints in a prototype
-- card's inset frame, which CR 702.160a offers as an alternative way to cast it.
--
-- NOT parametric in the keyword, unlike Pawl.Types.Equip and Pawl.Types.Cycling:
-- rule 718.1's inset frame holds a mana cost and a power/toughness box, none of
-- which can name a Filter or a Cost, so nothing here reaches back to
-- Pawl.Types.Keyword.
--
-- power and toughness are Integers rather than Pawl.Types.Power and
-- Pawl.Types.Toughness, whose payload is a Quantity: CR 718.1's inset box is a
-- printed number, and CR 208.2's star is a characteristic-defining ability the
-- card prints once, in its normal box. Integer and not Natural because
-- Pawl.Types.ProjectedCharacteristics.power is what these are read into.
data Prototype = MkPrototype
  { cost :: ManaCost.ManaCost,
    power :: Integer,
    toughness :: Integer
  }
  deriving (Eq, Ord, Show)

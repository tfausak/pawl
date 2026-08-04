module Pawl.Types.SubtypeFamily where

-- | CR 612.2: which kind of word a text-changing effect replaces. "A
-- text-changing effect changes only those words that are used in the correct
-- way (for example, a Magic color word being used as a color word, a land type
-- word used as a land type, or a creature type word used as a creature type)",
-- so the words a given text-changer offers are one family and not another.
--
-- Named for the words a card's own text names, which is what a player is asked
-- for: Magical Hack says "one basic land type", Artificial Evolution says "one
-- creature type". CR 205.3 is where the families are enumerated, and
-- Pawl.Engine.Subtype is where pawl classifies a word into one.
--
-- Not a color word, which CR 612.2 lists first: Sleight of Mind changes one,
-- and no card in the pool does. A color word swap is not a subtype swap at all
-- -- it would store something other than a ChangeSubtypeWord -- so it is not a
-- missing constructor here.
data SubtypeFamily
  = -- | CR 305.6's five: "The basic land types are Plains, Island, Swamp,
    -- Mountain, and Forest. If an object uses the words 'basic land type,' it's
    -- referring to one of these subtypes." A subset of CR 205.3i's land types,
    -- which is the family CR 612.2 gates on.
    BasicLandType
  | -- | CR 205.3m's list, which CR 612.2 names as "a creature type word used as
    -- a creature type".
    CreatureType
  deriving (Eq, Ord, Show)

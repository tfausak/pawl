module Pawl.Types.SubtypeFamily where

-- | CR 612.2: which kind of word a text-changing effect replaces, since a
-- text-changer changes only words used in the correct way. Named for the words a
-- card's own text names -- Magical Hack says "one basic land type", Artificial
-- Evolution "one creature type". CR 205.3 enumerates the families, and
-- Pawl.Engine.Subtype classifies a word into one.
--
-- Not a color word, which CR 612.2 lists first: no card in the pool changes one,
-- and such a swap would store something other than a ChangeSubtypeWord, so it is
-- not a missing constructor here.
data SubtypeFamily
  = -- | CR 305.6's five, a subset of CR 205.3i's land types, which is the family
    -- CR 612.2 gates on.
    BasicLandType
  | -- | CR 205.3m's list, CR 612.2's "creature type word used as a creature
    -- type".
    CreatureType
  deriving (Eq, Ord, Show)

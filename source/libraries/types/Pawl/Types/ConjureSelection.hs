module Pawl.Types.ConjureSelection where

-- | Which question an Alchemy conjure asks when its candidates are a printed
-- spellbook rather than one named card.
--
-- Conjure is a DIGITAL-ONLY keyword action and is in no rule of the CR --
-- @docs\/rules.txt@ does not contain the word -- so the authority for this type
-- is the printed sentence, 'Pawl.Types.ConjureDestination''s reason: a spellbook
-- conjure says either "a random card" or "a card of your choice", and this is
-- that half of the sentence.
--
-- A named sum rather than a Bool, 'Pawl.Types.ForageMode''s posture, so a
-- transcript reads as the question it records. The distinction is
-- observable rather than cosmetic: CR 701.9b's "at random" is not a choice, so
-- the two raise different prompts and a transcript of one must not satisfy the
-- other.
data ConjureSelection
  = -- | Tome of the Infinite\'s "conjure a random card from Tome of the
    -- Infinite\'s spellbook into your hand".
    AtRandom
  | -- | Follow the Tracks\'s "conjure a card of your choice from Follow the
    -- Tracks\'s spellbook onto the battlefield".
    ByChoice
  deriving (Bounded, Enum, Eq, Ord, Show)

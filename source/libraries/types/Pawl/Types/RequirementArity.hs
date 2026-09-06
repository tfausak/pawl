module Pawl.Types.RequirementArity where

-- | CR 508.1d / 509.1c: how many requirements one printed sentence states over
-- the creatures its subject clause names.
--
-- Both rules count REQUIREMENTS being obeyed, and a sentence naming several
-- creatures can mean either of two things by that. Lure means one requirement
-- each; Gaea's Protector means one requirement between them. Nothing about the
-- subject clause says which, so it is its own axis on
-- Pawl.Types.BlockRequirement and Pawl.Types.AttackRequirement.
data RequirementArity
  = -- | One requirement per creature the subject names -- Lure, Curse of the
    -- Nightly Hunt. CR 509.1c and CR 508.1d both check "each creature".
    EachSubject
  | -- | ONE requirement over the whole subject set, obeyed by any single member
    -- -- Gaea's Protector's "this creature must be blocked if able", Seeker of
    -- Slaanesh's "each opponent must attack with at least one creature each
    -- combat if able".
    AnySubject
  deriving (Bounded, Enum, Eq, Ord, Show)

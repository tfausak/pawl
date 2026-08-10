module Pawl.Types.KeywordFamily where

-- | CR 702. WHICH KEYWORD a Pawl.Types.Keyword is, with its payload dropped --
-- what card text means by "a creature with toxic" or "a creature with a morph
-- ability", as against "a creature with toxic 2".
--
-- The CR draws this line itself, in both places the cards come from. CR 702.14a:
-- "Landwalk is a GENERIC TERM that appears within an object's rules text as
-- '[type]walk'" -- the generic term and the written instance are different things
-- by the rule's own wording. CR 702.164a: "Toxic is a static ability. It is
-- written 'toxic N,' where N is a number" -- the ability is toxic, and N is how
-- it is written. So this type names the ability and Pawl.Types.Keyword names the
-- written instance; Pawl.Types.Filter asks for either.
--
-- One constructor per PAYLOAD-CARRYING Keyword constructor, and deliberately none
-- for the nullary ones. A nullary keyword's family would be the keyword, so
-- Filter.HasKeywordFamily would say exactly what Filter.HasKeyword already says
-- and "a creature with flying" would have two spellings. As it stands the two
-- atoms partition: HasKeyword Flying is the only way to write CR 702.9's
-- question, and HasKeywordFamily Toxic the only way to write CR 702.164's.
--
-- PAYLOAD-FREE, which is the whole point. A family designator that carried a
-- representative keyword -- HasKeywordFamily (Toxic 2) meaning "any toxic" --
-- would make two structurally different values observably equal, which is the
-- objection that sank widening HasKeyword in place (#522).
--
-- It imports NOTHING, and that is load-bearing rather than incidental:
-- Pawl.Types.Filter can name this type concretely while keeping the `keyword`
-- parameter that CR 702.14c's landwalk filter and CR 702.37a's morph cost force
-- on it. A family type that named Keyword instead would reopen that cycle.
--
-- FOURTEEN is this pool's count, not Magic's. Rule 702 runs to 702.194 and so
-- states 193 keywords past its own general 702.1, roughly a third of them written
-- with a cost or an N; of the keywords Pawl.Types.Keyword models, these fourteen
-- carry a payload. The set grows with that type -- ward N and
-- the alternative-cost keywords all land here eventually -- so a constructor is
-- owed whenever a payload-carrying Keyword constructor is added, not whenever a
-- card first asks for one. Pawl.Engine.Keyword.familyOf is exhaustive and takes
-- no wildcard, so the compiler asks for the decision.
data KeywordFamily
  = -- | CR 702.11d: hexproof from [quality].
    Hexproof
  | -- | CR 702.14a: "[type]walk".
    Landwalk
  | -- | CR 702.23a: rampage N.
    Rampage
  | -- | CR 702.29a: cycling [cost], and CR 702.29e's typecycling.
    Cycling
  | -- | CR 702.34a: flashback [cost].
    Flashback
  | -- | CR 702.37a: morph [cost]. CR 702.37e writes this family in the CR's own
    -- voice -- "a face-down permanent you control with A MORPH ABILITY" -- and
    -- Backslide's "target creature with a morph ability" is the card asking
    -- (#920).
    Morph
  | -- | CR 702.42a: entwine [cost].
    Entwine
  | -- | CR 702.45a: bushido N.
    Bushido
  | -- | CR 702.70a: poisonous N.
    Poisonous
  | -- | CR 702.86a: annihilator N.
    Annihilator
  | -- | CR 702.112a: renown N.
    Renown
  | -- | CR 702.122a: crew N.
    Crew
  | -- | CR 702.130a: afflict N.
    Afflict
  | -- | CR 702.164a: toxic N. The family Flensing Raptor's "another target
    -- creature you control with toxic" names.
    Toxic
  deriving (Eq, Ord, Show)

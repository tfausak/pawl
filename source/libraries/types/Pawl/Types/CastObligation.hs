module Pawl.Types.CastObligation where

-- | CR 608.2g's two limbs: whether a cast a resolving effect offers is one the
-- caster may decline, or one they take if able. The rule carries both postures in
-- one sentence -- it "specifically instructs or allows a player to cast a spell
-- during resolution".
--
-- @Optional@ is CR 310.12b's and CR 702.94a's "you may cast it"; @Mandatory@ is
-- Wild Evocation's "that player casts it ... if able". Mandatory does NOT mean
-- the cast always happens: rule 601.3's prohibitions and an unpayable cost still
-- stop it, and CR 118.8c hands the question back where a mandatory additional
-- cost names cards of a stated quality in a hidden zone.
--
-- NOT Pawl.Types.Optionality, which is CR 603.5's printed "may" over a CLAUSE's
-- instructions and names the player it asks. This span is that ONE cast, and the
-- caster is Pawl.Types.OfferCast's own field, so there is nobody left for this
-- type to name -- which is why the split, and why this is Pawl.Types
-- .PayObligation's shape rather than Optionality's.
--
-- Not a Bool, for PayObligation's reason: @Mandatory@ says which limb of the rule
-- is in play where @True@ would say nothing.
data CastObligation
  = Mandatory
  | Optional
  deriving (Bounded, Enum, Eq, Ord, Show)

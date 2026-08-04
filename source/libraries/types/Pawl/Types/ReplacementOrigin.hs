module Pawl.Types.ReplacementOrigin where

-- | CR 614.15: whether a replacement effect is a SELF-REPLACEMENT effect or one
-- of the "other replacement effects" the same rule orders behind them.
--
-- PROVENANCE, not shape, which is why this rides the CARRIER
-- (Pawl.Types.ActiveReplacement, Pawl.Types.ReplacementCandidate) rather than
-- Pawl.Types.ReplacementEffect: Galvanic Blast's "deals 4 damage instead" and a
-- permanent whose static ability said the same thing about the same source would
-- be the identical DamageR value, and only the first is CR 614.15's.
--
-- A PERMANENT's static replacement ability is therefore always Other, which is a
-- rules fact: CR 604.2 has a static ability CREATE a continuous effect, some of
-- which are replacement effects, and CR 614.15
-- puts self-replacement effects outside that class.
--
-- Read only by Pawl.Engine.Replacement.bucketOf, which is CR 616.1a's step.
data ReplacementOrigin
  = SelfReplacement -- CR 614.15
  | Other -- CR 614.15's "other replacement effects"
  deriving (Eq, Ord, Show)

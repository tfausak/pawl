module Pawl.Types.ReplacementOrigin where

-- | CR 614.15: whether a replacement effect is a SELF-REPLACEMENT effect --
-- "an effect of a resolving spell or ability that replace part or all of that
-- spell or ability's own effect(s)" -- or one of the "other replacement
-- effects" the same rule's last sentence orders behind them.
--
-- PROVENANCE, not shape, which is why this rides the CARRIER
-- (Pawl.Types.ActiveReplacement, Pawl.Types.ReplacementCandidate) rather than
-- Pawl.Types.ReplacementEffect: Galvanic Blast's "deals 4 damage instead" and a
-- hypothetical permanent whose static ability said the same thing about the same
-- source would be the identical DamageR value, and only the first is CR 614.15's.
-- Nothing in the payload can tell them apart, because the difference is which
-- ability created the effect.
--
-- A PERMANENT's static replacement ability is therefore always Other, and that is
-- a rules fact rather than an engine convenience: CR 614.15's first sentence says
-- self-replacement effects "are not continuous effects", and a static ability
-- generates exactly one of those (CR 611.1).
--
-- Read only by Pawl.Engine.Replacement.bucketOf, which is CR 616.1a's step --
-- "if any of the replacement and/or prevention effects are self-replacement
-- effects (see rule 614.15), one of them must be chosen."
data ReplacementOrigin
  = SelfReplacement -- CR 614.15
  | Other -- CR 614.15's "other replacement effects"
  deriving (Eq, Ord, Show)

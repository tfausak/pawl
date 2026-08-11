module Pawl.Types.TargetRequirement where

-- | CR 115.6: whether a target slot must be filled. "A spell or ability that
-- requires targets may allow zero targets to be chosen. Such a spell or ability
-- is still said to require targets, but that spell or ability is targeted only
-- if one or more targets have been chosen for it."
--
-- The two constructors are the two printed phrasings: "target creature" and "up
-- to one target creature". Both still DECLARE a slot, which is what keeps CR
-- 601.2c's announcement and CR 608.2b's re-validation reading one map.
--
-- 'UpToOne' and not a general count, because the count is a different gap: a slot
-- holds one Recipient, so "up to three target cards" needs the binding to hold a
-- set (#1219 states the decomposition).
data TargetRequirement
  = Required
  | UpToOne
  deriving (Eq, Ord, Show)

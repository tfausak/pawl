module Pawl.Types.Regenerability where

-- | CR 701.19c: whether a would-be-destroyed event admits a regeneration shield.
--
-- A property of the DESTRUCTION, not of the permanent. Terror's "It can't be
-- regenerated" says nothing about the creature: the same creature, with the same
-- shield, regenerates from lethal damage moments later.
--
-- CR 701.19c does NOT stop a shield being created -- it stops the shield being
-- APPLIED. So this rides the proposed event, where a replacement candidate is
-- offered its chance, and an unapplied shield is never consumed.
--
-- Not a Bool, for the reason TapState and Sickness are not: at a call site
-- `destroy CantBeRegenerated oid` says which rule is in play.
--
-- Gates regeneration ONLY. CR 701.19c names regeneration shields specifically, so
-- a future destruction replacement that is not a regeneration (a totem-armour
-- shape) must still apply -- which is why the gate reads the DestructionRewrite
-- rather than suppressing the whole DestructionR class.
data Regenerability
  = Regenerable
  | CantBeRegenerated
  deriving (Eq, Ord, Show)

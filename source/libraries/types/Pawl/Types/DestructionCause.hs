module Pawl.Types.DestructionCause where

-- | CR 122.1c: how a would-be-destroyed event came about, at the one grain a
-- destruction replacement narrows by -- whether an EFFECT is what destroys the
-- permanent.
--
-- Pawl.Types.CounterCause's shape and its reason: nothing about a proposed
-- destruction says where it came from, since the object and the rule that buries
-- it afterwards are identical either way, so the caller carries it in. Unlike that
-- type neither arm names a player, because rule 122.1c asks only the one question.
--
-- A property of the DESTRUCTION rather than of the permanent, exactly as
-- Pawl.Types.Regenerability is, and beside it on the proposed event for the same
-- reason: it decides which candidates are OFFERED the event, so a candidate the
-- cause refuses is also never consumed.
--
-- Read by Pawl.Engine.Replacement.admits, and by nothing else.
data DestructionCause
  = -- | A resolving spell or ability's effect (CR 609.1) -- Doom Blade's
    -- "destroy target creature", which is Pawl.Engine.Event.destroy and
    -- destroyReturning.
    ByEffect
  | -- | A rule, acting on its own: CR 704.5g's lethal damage and CR 704.5h's
    -- deathtouch damage, the only destructions the rules perform without an
    -- effect, both raised by Pawl.Engine.Sba through
    -- Pawl.Engine.Event.destroyInBatch. CR 122.1c's replacement does not reach
    -- these; CR 701.19a's regeneration does, which is why the two arms of
    -- Pawl.Types.DestructionRewrite read this differently.
    ByRule
  deriving (Eq, Ord, Show)

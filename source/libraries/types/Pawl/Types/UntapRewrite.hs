module Pawl.Types.UntapRewrite where

-- | CR 614.1a / 122.1d: how a replacement rewrites a would-become-untapped
-- event. Under its one arm the untap itself does not happen, so the permanent is
-- left exactly as tapped as it was.
--
-- One constructor, and a type rather than a bare
-- Pawl.Types.ReplacementEffect arm without a payload, for the reason that type's
-- own comment gives: a replacement effect is classified by the event class it
-- intercepts AND the rewrite shape it applies, and this rewrite is not nothing.
-- (PhaseR is the one arm with no rewrite, and CR 614.1b is why -- a skip really
-- does replace the event with nothing.)
data UntapRewrite
  = -- | CR 122.1d: "instead remove a stun counter from it". Engine-minted from
    -- the counters (Pawl.Engine.Projection.stunOf), never authored -- the card
    -- that puts the counter on says nothing about the effect it creates, rule
    -- 122.1d does. Pawl.CardSpec's engineOnlyOffends rejects the whole class for
    -- that reason, where CR 122.1c's destruction half needs a per-rewrite test
    -- because printed regeneration shares its arm.
    RemoveStunCounter
  deriving (Bounded, Enum, Eq, Ord, Show)

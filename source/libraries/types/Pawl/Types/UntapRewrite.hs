module Pawl.Types.UntapRewrite where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword

-- | CR 614.1a / 122.1d: how a replacement rewrites a would-become-untapped
-- event.
--
-- A type rather than a bare Pawl.Types.ReplacementEffect arm without a payload,
-- for the reason that type's own comment gives: a replacement effect is
-- classified by the event class it intercepts AND the rewrite shape it applies,
-- and this rewrite is not nothing. (PhaseR is the one arm with no rewrite, and
-- CR 614.1b is why -- a skip really does replace the event with nothing.)
data UntapRewrite
  = -- | CR 122.1d: "instead remove a stun counter from it". Engine-minted from
    -- the counters (Pawl.Engine.Projection.stunOf), never authored -- the card
    -- that puts the counter on says nothing about the effect it creates, rule
    -- 122.1d does. Pawl.EffectLintSpec's engineOnlyOffends rejects it.
    RemoveStunCounter
  | -- | CR 614.1a: "remove a +1/+1 counter from it instead. If you do, untap it"
    -- (Bewitching Leechcraft). With none of the kind to remove it stays tapped.
    RemoveCounterToUntap (CounterKind.CounterKind Keyword.Keyword)
  deriving (Eq, Ord, Show)

module Pawl.Types.Expiry where

import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 611.2: how long a STORED effect lasts, as the game remembers it. The
-- runtime counterpart of the printed Pawl.Types.Duration: card data says "until
-- your next turn", the game remembers WHOSE. Never appears in card JSON -- only
-- Pawl.Engine.Expiry.arm makes one -- but it does have a runtime codec, since
-- CR 603.7b lets a delayed ability state a duration that must survive the trip.
--
-- The split is what makes a card-only value in GameState unrepresentable: with
-- one type carrying both, a printed arm leaking into a stored effect would match
-- no sweep and last forever, silently. Only Pawl.Engine.Expiry may case on this.
data Expiry
  = -- | CR 514.2: "all 'until end of turn' and 'this turn' effects end" during
    -- the cleanup step.
    AtCleanup
  | -- | CR 611.2a: "lasts until the end of the game". No sweep ends it.
    Never
  | -- | CR 611.2b: "for as long as ...". The PlayerId is CR 109.5's "you", baked
    -- in by Pawl.Engine.Expiry.arm at the moment the effect is stored -- derived from
    -- the effect's controller, never chosen. The duration is ONE continuous
    -- period: once the condition stops holding the effect is DELETED, and a
    -- condition that becomes true again does not bring it back.
    --
    -- The CONDITION is baked at the same moment (Pawl.Engine.Condition.bakeBound):
    -- a PlayerRef naming one of the resolution's slots becomes PlayerRef.Specific,
    -- since the sweep that re-reads this has no resolution to read a slot off.
    While PlayerId.PlayerId Condition.Condition
  | -- | CR 611.2a: "until your next turn", as a concrete player. Ends as that
    -- player's turn begins.
    AtTurnOf PlayerId.PlayerId
  | -- | CR 500.5: effects lasting until the end of a step or phase expire as it
    -- ends. A Pawl.Types.PhaseSelector because CR 500.5 names both grains and
    -- that type spans both.
    --
    -- Matched by EQUALITY against the ending window, so a selector naming the
    -- combat phase is never confused with one naming a step of it -- CR 500.5a
    -- puts "until end of combat" at the end of the combat PHASE, so the end of
    -- combat step's own end does not reach an AtEndOf CombatPhase entry.
    --
    -- Only the CombatPhase arm has a printed producer today
    -- (Duration.UntilEndOfCombat, Jade Statue); the others cost nothing.
    AtEndOf PhaseSelector.PhaseSelector
  deriving (Eq, Ord, Show)

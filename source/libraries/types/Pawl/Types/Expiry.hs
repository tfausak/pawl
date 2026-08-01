module Pawl.Types.Expiry where

import Pawl.Types.Condition (Condition)
import Pawl.Types.PhaseSelector (PhaseSelector)
import Pawl.Types.PlayerId (PlayerId)

-- CR 611.2: how long a STORED effect lasts, as the game remembers it. The
-- runtime counterpart of the printed Pawl.Types.Duration, the same way
-- ActiveReplacement is ReplacementEffect's and PendingTrigger is
-- DelayedTrigger's: card data says "until your next turn", the game remembers
-- WHOSE. Never appears in card JSON: a card writes a Duration, and only
-- Pawl.Engine.Expiry.arm makes one of these. It does have a runtime codec, whose one
-- caller is a DelayedTrigger's -- CR 603.7b lets a delayed ability state a
-- duration, so a stored one has to survive the trip.
--
-- The split is what makes a card-only value in GameState unrepresentable. With
-- one type carrying both, a printed arm that leaked into a stored effect would
-- match no sweep and the effect would last forever -- silently. Only
-- Pawl.Engine.Expiry may case on this type.
data Expiry
  = -- CR 514.2: "all 'until end of turn' and 'this turn' effects end" during
    -- the cleanup step.
    AtCleanup
  | -- CR 611.2a: "lasts until the end of the game". No sweep ends it.
    Never
  | -- CR 611.2b: "for as long as ...". The PlayerId is CR 109.5's "you", baked
    -- in by Pawl.Engine.Expiry.arm at the moment the effect is stored -- derived from
    -- the effect's controller, never chosen. The duration is ONE continuous
    -- period: once the condition stops holding the effect is DELETED, and a
    -- condition that becomes true again does not bring it back.
    While PlayerId Condition
  | -- CR 611.2a: "until your next turn", as a concrete player. Ends as that
    -- player's turn begins.
    AtTurnOf PlayerId
  | -- CR 500.5: "As a step or phase ends, if there are effects that last until
    -- the end of that step or phase, those effects expire." The window is a
    -- Pawl.Types.PhaseSelector because CR 500.5 names BOTH grains in one
    -- sentence and that type already spans both -- `Step` for one schedule
    -- entry, the other three arms for CR 500.1's stepped phases.
    --
    -- Matched by EQUALITY against the window that is ending, which is the same
    -- comparison Pawl.Engine.Replacement makes for a skip (CR 614.1b): a
    -- selector naming the combat phase can never be confused with one naming a
    -- step of it. That is exactly what CR 500.5a demands -- "effects that last
    -- 'until end of combat' expire at the end of the combat PHASE, not at the
    -- beginning of the end of combat step" -- so the step's own end does not
    -- reach an AtEndOf CombatPhase entry.
    --
    -- Only the CombatPhase arm has a printed producer today
    -- (Duration.UntilEndOfCombat, Jade Statue). The others cost nothing: this
    -- is one payload of a type Pawl.Engine.Expiry already had to case on, and
    -- the sweep is one equality either way.
    AtEndOf PhaseSelector
  deriving (Eq, Ord, Show)

module Pawl.Types.ManaAbilityPerformer where

import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Game as Game
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 405.6c: how the closed half runs the NON-MANA effects of a mana ability
-- -- the ability's source, its controller, and the effects themselves. CR 605.3b
-- gives such an ability no stack object, so nothing above resolves it and the
-- payment path has to run it where it stands.
--
-- A PARAMETER rather than an import because Pawl.Engine.Resolve sits ABOVE
-- Pawl.Engine.Cost in the module graph, so importing back would close a cycle.
-- Resolve.resolveSpellWith takes its subgame runner and Pawl.Engine.Mulligan its
-- hand-action performer the same way (Pawl.Types.HandActionPerformer).
--
-- Taken as a parameter by the modules Pawl.Engine.Resolve imports and cannot
-- take one; the rest -- Pawl.Engine.Activate, Foretell, Ignore, EndEffect,
-- Action, Engine -- import Resolve and read Resolve.performManaAbility off it.
--
-- Deliberately has NO default: "no performer" is not a real state of the world,
-- and one would silently drop the damage Ancient Tomb charges for its mana at
-- whichever call site forgot it.
--
-- A RECORD of two functions rather than one, so that CR 605.4a's triggered mana
-- ability rides the same injection: the payment path applies that one too, and
-- it too resolves through Pawl.Engine.Resolve, so a parameter of its own would
-- have to be threaded through every caller of Pawl.Engine.Cost.pay for no gain.
data ManaAbilityPerformer = MkManaAbilityPerformer
  { -- | CR 405.6c: the ability's source, its controller, and the non-mana
    -- effects to run.
    effects :: ObjectId.ObjectId -> PlayerId.PlayerId -> [Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card)] -> Game.Game (),
    -- | CR 605.4a: apply one triggered mana ability where it stands, without
    -- putting it on the stack. Pawl.Engine.Cost.tapForManaWith gathers and
    -- classifies (Pawl.Engine.ManaAbility.isTriggeredManaAbility); this runs the
    -- ability's effects.
    triggered :: PendingTrigger.PendingTrigger -> Game.Game ()
  }

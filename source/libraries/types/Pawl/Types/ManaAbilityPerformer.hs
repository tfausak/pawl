module Pawl.Types.ManaAbilityPerformer where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Game as Game
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName

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
-- Built by Pawl.Engine.Resolve.Effect.performManaAbility around CR 729.1a's
-- subgame runner, which a triggered mana ability can need (CR 605.4a);
-- Pawl.Engine.Engine.performManaAbility is the live loop's, and every module
-- below the loop takes the record or the runner as a parameter.
--
-- Deliberately has NO default: "no performer" is not a real state of the world,
-- and one would silently drop the damage Ancient Tomb charges for its mana at
-- whichever call site forgot it.
--
-- A RECORD rather than one function, so that CR 605.4a's triggered mana ability
-- and CR 118.12's resolution cost ride the same injection: the payment path
-- needs both too, and both live in Pawl.Engine.Resolve, so a parameter of each
-- own would have to be threaded through every caller of Pawl.Engine.Cost.pay for
-- no gain.
data ManaAbilityPerformer = MkManaAbilityPerformer
  { -- | CR 405.6c / 608.2c: the ability's source, its controller, the slots
    -- its earlier clauses bound, and the non-mana effects to run; answers those
    -- slots with the ones these effects bound added.
    effects :: ObjectId.ObjectId -> PlayerId.PlayerId -> Map.Map SlotName.SlotName (Set.Set Recipient.Recipient) -> [Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card)] -> Game.Game (Map.Map SlotName.SlotName (Set.Set Recipient.Recipient)),
    -- | CR 605.4a: apply one triggered mana ability where it stands, without
    -- putting it on the stack. Pawl.Engine.Cost.tapForManaWith gathers and
    -- classifies (Pawl.Engine.ManaAbility.isTriggeredManaAbility); this runs the
    -- ability's effects.
    triggered :: PendingTrigger.PendingTrigger -> Game.Game (),
    -- | CR 118.12: offer one clause's resolution cost -- the source, its
    -- controller, the clause's ordinal and gate, and the answers already
    -- recorded by ordinal (PayGate.offeredAt) -- and say whether the clause
    -- happens. Rhystic Cave's "unless any player pays {1}".
    payGate :: ObjectId.ObjectId -> PlayerId.PlayerId -> ClauseIndex.ClauseIndex -> PayGate.PayGate -> Map.Map ClauseIndex.ClauseIndex (Map.Map PlayerId.PlayerId Bool) -> Game.Game (Bool, Map.Map ClauseIndex.ClauseIndex (Map.Map PlayerId.PlayerId Bool))
  }

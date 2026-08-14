-- CR 611.2: the life cycle of a stored effect's duration. The ONLY module that
-- may case on Pawl.Types.Expiry -- the standing Pawl.Engine.Resolve has over
-- Effect and Pawl.Engine.Projection over Modification. It owns the
-- transformation from the PRINTED Duration to the STORED Expiry (`arm`) and
-- every sweep that ends one, over seven carriers that share one expiry
-- vocabulary and so share one sweep. Two of the seven carry MAYBE an expiry
-- rather than one outright, for different reasons: a delayed trigger may state
-- no duration at all (CR 603.7b), and an object's play permission (CR 601.3,
-- Object.playableFromExile) is usually absent entirely -- so where the other
-- five carriers are DROPPED from a list, the permission is CLEARED on an object
-- that stays.
--
-- One of them, CR 116.2d's ignore, is the one that SUPPRESSES rather than adds;
-- its duration is a duration all the same, and it is swept as one.
module Pawl.Engine.Expiry where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AfterTurn as AfterTurn
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import Pawl.Types.Duration (Duration)
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import Pawl.Types.Expiry (Expiry)
import qualified Pawl.Types.Expiry as Expiry
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PhaseSelector (PhaseSelector)
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.While as While

-- CR 611.2: the moment a duration BEGINS. `controller` is the effect's
-- controller -- CR 109.5's "you" -- and `source` is the object the effect comes
-- from. Nothing means the duration never started, so per CR 611.2b the effect
-- does nothing and is never stored at all.
--
-- `players` is the resolution's binding environment, projected to the seats it
-- names (Binding.playersIn). It is what a CR 611.2b condition saying "that
-- player" is baked against, HERE and only here -- see the ForAsLongAs arm. Empty
-- for a caller with no resolution behind it, whose durations name no slot.
--
-- CR 611.2b's second sentence is vacuous here: this runs once, at the point the
-- effect would be stored, and no opcode both ends and restarts a condition
-- mid-resolution.
arm :: Map.Map SlotName PlayerId -> PlayerId -> ObjectId -> Duration -> GameState -> Maybe Expiry
arm players controller source duration gs = case duration of
  Duration.UntilEndOfTurn -> Just Expiry.AtCleanup
  Duration.Indefinite -> Just Expiry.Never
  Duration.UntilYourNextTurn -> Just (Expiry.AtTurnOf controller)
  -- CR 611.2a: "until the end of your next turn". Two samples, both taken here
  -- and neither ever rewritten: the controller (CR 109.5's "you", as above) and
  -- the turn this duration began on. dropAtCleanup ends it at the first turn of
  -- the controller's numbered ABOVE that one -- so a duration that began during
  -- their own turn survives that turn's cleanup, which is the whole difference
  -- between this arm and the one above.
  Duration.UntilEndOfYourNextTurn ->
    Just (Expiry.AtEndOfTurnOf (AfterTurn.MkAfterTurn controller (GameState.turnNumber gs)))
  -- BAKED, and stored baked: the condition outlives the resolution that stored
  -- it, and sweepConditional below re-reads it off the effect's
  -- SOURCE, whose bindings never held the resolution's slots. An InSlot left
  -- standing would answer Nothing there, Condition.holds would collapse that to
  -- False, and the effect would silently end at the first settle -- or, for an
  -- ability, never start at all, since the same unresolvable reference is read
  -- one line below.
  Duration.ForAsLongAs cond ->
    let baked = Condition.bakeBound players cond
     in if Condition.holds (Projection.fullView gs) (Filter.contextFor (Just controller) (Just source)) gs source baked
          then Just (Expiry.While (While.MkWhile controller baked))
          else Nothing
  -- CR 500.5a / 511.2: "until end of combat" is the end of the combat PHASE, so
  -- the stored window is PhaseSelector.CombatPhase and never the end of combat
  -- step. Naming the phase is the whole of the arming: unlike UntilYourNextTurn
  -- there is nothing about the game to bake in, because the sweep ends the
  -- effect at the first combat phase whose end it sees (#525).
  Duration.UntilEndOfCombat -> Just (Expiry.AtEndOf PhaseSelector.CombatPhase)

-- CR 514.2: "until end of turn" and "this turn" effects end during the cleanup
-- step. Delete-and-recompute (design.md 2.5): dropping the stored entry makes
-- the next projection revert -- nothing is explicitly undone.
--
-- Not implemented: a turn whose ENDING PHASE was skipped never reaches the
-- cleanup step, so this never runs and every AtCleanup entry outlives its turn
-- (#491).
dropAtCleanup :: GameState -> GameState
dropAtCleanup gs =
  let survives expiry = case expiry of
        Expiry.AtCleanup -> False
        Expiry.Never -> True
        Expiry.While {} -> True
        Expiry.AtTurnOf _ -> True
        -- CR 611.2a: "until the end of your next turn" ends as that turn ends,
        -- which is this same CR 514.2 moment -- so the sweep that ends an
        -- until-end-of-turn effect is the one that ends this too, one named turn
        -- later. The turn is named by the pair (see Pawl.Types.AfterTurn): this
        -- cleanup belongs to that player, and its number is above the one the
        -- duration began on, so it is not the duration's own turn.
        Expiry.AtEndOfTurnOf afterTurn ->
          AfterTurn.player afterTurn /= GameState.activePlayer gs
            || GameState.turnNumber gs <= AfterTurn.turn afterTurn
        Expiry.AtEndOf _ -> True
      keepEffect eff = survives (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.expiry active)
      keepPlayerEffect active = survives (ActivePlayerEffect.expiry active)
      keepBlockRequirement active = survives (ActiveBlockRequirement.expiry active)
      keepDelayed = maybe True survives . DelayedTrigger.expiry
      -- CR 116.2d: an ignore is stored "for a duration" like every carrier
      -- above, and every printed one says until end of turn -- so Leonin Arbiter
      -- stops the next turn's searches again, with nothing to reinstate.
      keepIgnored = survives . IgnoredAbility.expiry
   in gs
        { GameState.continuousEffects = filter keepEffect (GameState.continuousEffects gs),
          GameState.replacements = filter keepReplacement (GameState.replacements gs),
          GameState.playerEffects = filter keepPlayerEffect (GameState.playerEffects gs),
          GameState.blockRequirements = filter keepBlockRequirement (GameState.blockRequirements gs),
          GameState.ignoredAbilities = filter keepIgnored (GameState.ignoredAbilities gs),
          GameState.delayedTriggers = Seq.filter keepDelayed (GameState.delayedTriggers gs),
          GameState.objects = clearedPermissions (survives . ExilePlayPermission.expiry) gs
        }

-- CR 611.2b: drop every While whose condition has stopped holding. The effect
-- is DELETED, not masked: the duration is one continuous period, so an effect
-- that has ended stays ended even if the condition becomes true again. Reports
-- whether it changed anything, so Engine.settleForPriority knows to run again.
--
-- CR 704.3 fixes the coarsest moment anything can OBSERVE the condition, and
-- settleForPriority runs at exactly the points where the board can change, so
-- checking here is indistinguishable from checking continuously.
--
-- `filter` only removes elements and preserves the survivors' order, so a
-- LENGTH compare is equivalent to a deep structural `/=` and cheaper. The
-- permission carrier answers the same question with anyPermissionEnded, which is
-- a scan and not a rebuild.
-- `State.put` is skipped when nothing changed, so a no-op sweep does not
-- rewrite the GameState.
sweepConditional :: Game Bool
sweepConditional = do
  gs <- State.get
  let survives source expiry = case expiry of
        Expiry.While (While.MkWhile you cond) -> Condition.holds (Projection.fullView gs) (Filter.contextFor (Just you) (Just source)) gs source cond
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        Expiry.AtTurnOf _ -> True
        Expiry.AtEndOfTurnOf _ -> True
        Expiry.AtEndOf _ -> True
      keepEffect eff = survives (ContinuousEffect.source eff) (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.source active) (ActiveReplacement.expiry active)
      keepPlayerEffect active = survives (ActivePlayerEffect.source active) (ActivePlayerEffect.expiry active)
      keepBlockRequirement active = survives (ActiveBlockRequirement.source active) (ActiveBlockRequirement.expiry active)
      keepDelayed entry = maybe True (survives (DelayedTrigger.source entry)) (DelayedTrigger.expiry entry)
      keptEffects = filter keepEffect (GameState.continuousEffects gs)
      keptReplacements = filter keepReplacement (GameState.replacements gs)
      keptPlayerEffects = filter keepPlayerEffect (GameState.playerEffects gs)
      keptBlockRequirements = filter keepBlockRequirement (GameState.blockRequirements gs)
      keptDelayed = Seq.filter keepDelayed (GameState.delayedTriggers gs)
      keepIgnored ignored = survives (IgnoredAbility.source ignored) (IgnoredAbility.expiry ignored)
      keptIgnored = filter keepIgnored (GameState.ignoredAbilities gs)
      keepPermission permission = survives (ExilePlayPermission.source permission) (ExilePlayPermission.expiry permission)
      keptObjects = clearedPermissions keepPermission gs
      changed =
        length keptEffects /= length (GameState.continuousEffects gs)
          || length keptReplacements /= length (GameState.replacements gs)
          || length keptPlayerEffects /= length (GameState.playerEffects gs)
          || length keptBlockRequirements /= length (GameState.blockRequirements gs)
          || Seq.length keptDelayed /= Seq.length (GameState.delayedTriggers gs)
          || length keptIgnored /= length (GameState.ignoredAbilities gs)
          -- Omitting this term would be silent: settleForPriority would not run
          -- again, and a permission whose loss changes what a player may do would
          -- be observed one settle late.
          || anyPermissionEnded keepPermission gs
  Monad.when changed $
    State.put
      gs
        { GameState.continuousEffects = keptEffects,
          GameState.replacements = keptReplacements,
          GameState.playerEffects = keptPlayerEffects,
          GameState.blockRequirements = keptBlockRequirements,
          GameState.ignoredAbilities = keptIgnored,
          GameState.delayedTriggers = keptDelayed,
          GameState.objects = keptObjects
        }
  pure changed

-- Has any permission on the board ended under this test? The question the whole
-- carrier is asked through, and it is a SCAN rather than a rebuild: almost every
-- board carries no permission at all, and sweepConditional runs at every settle.
anyPermissionEnded :: (ExilePlayPermission.ExilePlayPermission -> Bool) -> GameState -> Bool
anyPermissionEnded survives gs =
  any (maybe False (not . survives) . Object.playableFromExile) (GameState.objects gs)

-- CR 601.3's permission, ended. CLEARED rather than dropped, which is the one
-- way this carrier differs from the other four: they are entries in a list that
-- goes away, and this is a field on an object that stays. Nothing else about the
-- object is touched, an object with no permission is left exactly as it was, and
-- the map is rebuilt only when something actually ended.
clearedPermissions :: (ExilePlayPermission.ExilePlayPermission -> Bool) -> GameState -> Map.Map ObjectId Object.Object
clearedPermissions survives gs =
  let clear object =
        if maybe True survives (Object.playableFromExile object)
          then object
          else object {Object.playableFromExile = Nothing}
   in if anyPermissionEnded survives gs
        then Map.map clear (GameState.objects gs)
        else GameState.objects gs

-- CR 611.2a: a duration a spell or ability states lasts as long as it says, so
-- an until-your-next-turn duration ends as that player's turn begins.
--
-- Takes the player EXPLICITLY rather than reading GameState.activePlayer,
-- because CR 800.4m needs this to fire for a seat whose turn does not begin: a
-- departed player's durations last until their turn WOULD have begun. Engine's
-- turn handoff walks the seating order and calls this at every seat it passes.
--
-- Dropping at the handoff is observably identical to dropping "as the turn
-- begins": CR 500.12, CR 502.4 and CR 704.3 leave nothing that could observe
-- the difference. The first observation point is the upkeep step (CR 503.1).
dropAtTurnOf :: PlayerId -> GameState -> GameState
dropAtTurnOf pid gs =
  let -- CR 800.4m: "or until a specific point in that turn". A departed player's
      -- turn never begins, so the cleanup sweep that would end an
      -- until-the-END-of-their-next-turn effect never runs and the effect would
      -- last for the rest of the game. The rule ends it here instead, at the
      -- point that turn would have begun -- the same moment, and the same call,
      -- as the arm above. For a player still in the game this is the beginning of
      -- a turn their effect is meant to survive, so it is left alone.
      departed = List.notElem pid (Game.stillPlaying gs)
      survives expiry = case expiry of
        Expiry.AtTurnOf p -> p /= pid
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        Expiry.While {} -> True
        Expiry.AtEndOfTurnOf afterTurn -> not (departed && AfterTurn.player afterTurn == pid)
        Expiry.AtEndOf _ -> True
      keepEffect eff = survives (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.expiry active)
      keepPlayerEffect active = survives (ActivePlayerEffect.expiry active)
      keepBlockRequirement active = survives (ActiveBlockRequirement.expiry active)
      keepDelayed = maybe True survives . DelayedTrigger.expiry
      keepIgnored = survives . IgnoredAbility.expiry
   in gs
        { GameState.continuousEffects = filter keepEffect (GameState.continuousEffects gs),
          GameState.replacements = filter keepReplacement (GameState.replacements gs),
          GameState.playerEffects = filter keepPlayerEffect (GameState.playerEffects gs),
          GameState.blockRequirements = filter keepBlockRequirement (GameState.blockRequirements gs),
          GameState.ignoredAbilities = filter keepIgnored (GameState.ignoredAbilities gs),
          GameState.delayedTriggers = Seq.filter keepDelayed (GameState.delayedTriggers gs),
          GameState.objects = clearedPermissions (survives . ExilePlayPermission.expiry) gs
        }

-- CR 500.5's first clause: effects lasting until the end of a step or phase
-- expire as it ends. The window that is ending is passed in, because only
-- Engine.runStepThatBegan knows which one it is -- and at the end of the last
-- step of a stepped phase there are TWO, the step and the phase, so it calls
-- this twice.
--
-- EQUALITY on the selector, not containment: CR 500.5a (repeated by CR 511.2)
-- is precisely the claim that an "until end of combat" effect does NOT expire
-- when the end of combat step ends as a step, and containment would end it at
-- the first combat step it saw. Pawl.Engine.Turn.inWindow is the containment
-- test, and it answers a different question for a different reader.
dropAtEndOf :: PhaseSelector -> GameState -> GameState
dropAtEndOf ending gs =
  let survives expiry = case expiry of
        Expiry.AtEndOf window -> window /= ending
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        Expiry.While {} -> True
        Expiry.AtTurnOf _ -> True
        Expiry.AtEndOfTurnOf _ -> True
      keepEffect eff = survives (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.expiry active)
      keepPlayerEffect active = survives (ActivePlayerEffect.expiry active)
      keepBlockRequirement active = survives (ActiveBlockRequirement.expiry active)
      keepDelayed = maybe True survives . DelayedTrigger.expiry
      keepIgnored = survives . IgnoredAbility.expiry
   in gs
        { GameState.continuousEffects = filter keepEffect (GameState.continuousEffects gs),
          GameState.replacements = filter keepReplacement (GameState.replacements gs),
          GameState.playerEffects = filter keepPlayerEffect (GameState.playerEffects gs),
          GameState.blockRequirements = filter keepBlockRequirement (GameState.blockRequirements gs),
          GameState.ignoredAbilities = filter keepIgnored (GameState.ignoredAbilities gs),
          GameState.delayedTriggers = Seq.filter keepDelayed (GameState.delayedTriggers gs),
          GameState.objects = clearedPermissions (survives . ExilePlayPermission.expiry) gs
        }

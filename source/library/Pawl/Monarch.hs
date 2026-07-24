module Pawl.Monarch where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import Pawl.Type.Binding (Binding)
import Pawl.Type.Card (Card)
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.EndingStep as EndingStep
import Pawl.Type.Game (Game)
import Pawl.Type.GameEvent (GameEvent)
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.MonarchTarget as MonarchTarget
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Phase as Phase
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import Pawl.Type.TriggerCondition (TriggerCondition)
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import Pawl.Type.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TurnScope as TurnScope
import qualified Pawl.Type.Zone as Zone

-- A single-mode, single-effect triggered ability (the shape all monarch inherent
-- abilities take): one Mode with no targets, forced (ChooseExactly 1). intervening
-- = Nothing is load-bearing: Stack.resolveTop's OfInherentTrigger arm skips the CR
-- 608.2a resolution recheck, which is sound ONLY because these abilities have no
-- intervening "if".
oneEffect :: TriggerCondition -> Effect.Effect Card -> TriggeredAbility Card
oneEffect cond eff =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = cond,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton eff) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing
    }

-- CR 725.2: "At the beginning of the monarch's end step, that player draws a
-- card." Controller-scoped to the monarch, so ControllersTurn + the monarch as
-- "you" is exactly the monarch's own end step.
endStepDraw :: TriggeredAbility Card
endStepDraw =
  oneEffect
    (TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.ControllersTurn)
    (Effect.Draw (Quantity.Literal 1))

-- CR 725.2: "Whenever a creature deals combat damage to the monarch, its
-- controller becomes the monarch." Controlled by the current monarch; makes a
-- DIFFERENT player (the damager's controller) the monarch.
crownSteal :: TriggeredAbility Card
crownSteal =
  oneEffect
    TriggerCondition.CreatureDealtCombatDamageToMonarch
    (Effect.BecomeMonarch MonarchTarget.ControllerOfSource)

-- The monarch's inherent abilities, present only while there is a monarch.
monarchAbilities :: [TriggeredAbility Card]
monarchAbilities = [endStepDraw, crownSteal]

-- CR 725.2: match one inherent ability against one event for the given monarch
-- (who is the ability's controller), yielding the placed ability's binding
-- environment (empty for the draw; the damaging creature under the reserved
-- trigger-source slot for the steal). Sourceless -- no bearer, so this is a
-- dedicated matcher, not Event.matchesTrigger. The GameState is read by the
-- steal arm (Projection.isCreatureOf), unused by the draw's match.
inherentMatch :: PlayerId -> TriggerCondition -> GameState -> GameEvent -> Maybe (Map SlotName.SlotName Binding)
inherentMatch monarch cond gs event = case (cond, event) of
  (TriggerCondition.StepBegins wanted scope, GameEvent.StepBegan began active)
    | began == wanted && scopeOk scope active -> Just Map.empty
  -- CR 725.2: a creature dealt COMBAT damage to the monarch. Bind the damaging
  -- creature under the reserved trigger-source slot so Effect.BecomeMonarch
  -- ControllerOfSource reads it and crowns THAT creature's controller.
  (TriggerCondition.CreatureDealtCombatDamageToMonarch, GameEvent.DamageDealt ev)
    | DamageEvent.kind ev == DamageKind.Combat
        && DamageEvent.target ev == Recipient.ToPlayer monarch
        && Projection.isCreatureOf (DamageEvent.source ev) gs ->
        Just (Binding.setTriggerSource (DamageEvent.source ev) Map.empty)
  _ -> Nothing
  where
    scopeOk s a = case s of
      TurnScope.EachTurn -> True
      TurnScope.ControllersTurn -> a == monarch

-- CR 725.1/725.2: the inherent triggers that fire on this batch of events, each
-- as (controller, ability, bindings). Empty when there is no monarch (the
-- abilities do not exist).
inherentMonarchPending :: [GameEvent] -> GameState -> [(PlayerId, TriggeredAbility Card, Map SlotName.SlotName Binding)]
inherentMonarchPending events gs = case GameState.monarch gs of
  Nothing -> []
  Just m ->
    let matchEvent ab ev = case inherentMatch m (TriggeredAbility.condition ab) gs ev of
          Nothing -> Nothing
          Just b -> Just (m, ab, b)
        forAbility ab = Maybe.mapMaybe (matchEvent ab) events
     in concatMap forAbility monarchAbilities

-- Mint a sourceless inherent trigger onto the stack (the placeOne analog for an
-- ability with no source object). Single mode, no targets -- so no mode/target
-- prompt; the single mode is selected outright (CR 725.2's draw is mandatory and
-- unmodal, so there is nothing to ask). The chosen modes ride under the reserved
-- chosenModes slot, which Stack.resolveTop's OfInherentTrigger arm reads to know
-- which effects to run.
placeInherent :: PlayerId -> TriggeredAbility Card -> Map SlotName.SlotName Binding -> Game ()
placeInherent controller ability provided = do
  gs <- State.get
  let (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      modeCount = Seq.length (Modal.modes (TriggeredAbility.modal ability))
      allModes = Set.fromList (fmap (ModeIndex.MkModeIndex . fromIntegral) [0 .. modeCount - 1])
      bindings = Binding.setYou controller (Map.union provided (Binding.fromChoices Map.empty Map.empty Nothing allModes))
      obj =
        Object.MkObject
          { Object.owner = controller,
            Object.source = Source.OfInherentTrigger controller ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = bindings,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
  State.put gs2 {GameState.objects = Map.insert abilId obj (GameState.objects gs2), GameState.stack = abilId : GameState.stack gs2}

-- CR 725 (Palace Jailer): return every "exiled until an opponent becomes the
-- monarch" object once an opponent of its controller is the monarch. Two-player
-- (CR 102.2): an opponent is any player other than the controller, so an entry
-- is due iff its controller is not the current monarch. Runs in the settle loop.
returnExiledForMonarch :: Game Bool
returnExiledForMonarch = do
  gs <- State.get
  case GameState.monarch gs of
    Nothing -> pure False
    Just m ->
      let due = Map.keys (Map.filter (/= m) (GameState.exiledUntilMonarch gs))
       in if null due
            then pure False
            else do
              Monad.forM_ due $ \oid -> do
                _ <- Event.changeZoneReturning oid Zone.Battlefield
                State.modify' (\g -> g {GameState.exiledUntilMonarch = Map.delete oid (GameState.exiledUntilMonarch g)})
              pure True

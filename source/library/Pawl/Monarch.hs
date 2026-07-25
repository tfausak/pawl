module Pawl.Monarch where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
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

-- CR 725 (Palace Jailer): return every object exiled "until an opponent becomes
-- the monarch" once an opponent of the entry's controller is the monarch. Runs in
-- the settle loop; CR 704.3 fixes "whenever a player would get priority" as the
-- coarsest moment anything can observe a condition, and Engine.settleForPriority
-- runs at exactly the points where the board can change.
--
-- "An opponent" is every player other than the controller. That is not a
-- two-player shortcut: in a free-for-all the players compete as individuals and
-- every other player is an opponent by construction (CR 806.1), so an entry is
-- due iff its controller is not the current monarch. CR 102.3 makes a TEAMMATE
-- not an opponent, and teams are the only reading this arm would be wrong for;
-- pawl has none (#175).
--
-- When the controller has LEFT the game, the set is resolved by CR 800.4i: "If an
-- effect requires information about a specific player, the effect uses the current
-- information about that player if they are still in the game; otherwise, the
-- effect uses the last known information about that player before they left the
-- game." Who a departed player's opponents were is information about a specific
-- player, so the set freezes at departure -- in a free-for-all, every other player
-- who was in the game. The same `/= m` filter computes it, because CR 725.4
-- guarantees the monarch is always a player still in the game and a departed
-- controller is therefore never the monarch. Nothing needs to be stored.
--
-- Departure.objectsLeaveWith drops an entry whose KEY -- the exiled object -- is
-- owned by a departing player, and never one whose VALUE is: the effect survives
-- its controller's departure. Palace Jailer's own ruling agrees the watch is not
-- tied to the source object.
--
-- The due test is a STATE check where the card's ruling asks for an EVENT -- a new
-- monarch being crowned who is an opponent, not merely an opponent currently
-- holding the crown. Reachable at two players by ordering Palace Jailer's two
-- entry triggers (#171). The CR 725.4 departure path above is unaffected: a
-- reassignment IS a new monarch being crowned, so both readings agree there.
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

-- CR 725.4: "If the monarch leaves the game, the active player becomes the
-- monarch at the same time as that player leaves the game. If the active player
-- is leaving the game or if there is no active player, the next player in turn
-- order who can become the monarch becomes the monarch. If no player still in
-- the game can become the monarch, the game continues with no monarch."
--
-- `playing` is the still-playing seats in SEATING order, injected by the caller
-- rather than computed here: Pawl.Departure calls this at the moment of
-- departure (CR 725.4's "at the same time as that player leaves the game"), so
-- this module may not import Pawl.Departure to ask. Same injected-value idiom
-- Pawl.Count uses for its ViewOf.
--
-- Engine.nextStillPlaying walks the same seating order, for CR 800.4a's
-- priority successor -- a different rule, but the same shape of walk -- and
-- this is deliberately NOT a call to it, for three reasons that would each
-- become a parameter if it were: it anchors on the ACTIVE seat and excludes
-- it (the active player's own case is the branch below), where
-- nextStillPlaying anchors on the DEPARTING seat and includes it in its wrap
-- so it can return that seat; this function is partial in the seat it finds
-- (`next :: Maybe PlayerId`), because CR 725.4's third sentence needs the
-- game to be able to continue with no monarch, where nextStillPlaying is
-- total; and `playing` is injected here (see above), where nextStillPlaying
-- reads Departure.stillPlaying directly, because this module may not import
-- Pawl.Departure. Changing either walk without checking the other risks
-- reintroducing this duplication with a mismatch baked in.
--
-- Called with `leaving` ALREADY marked departed, so "is leaving the game" and
-- "has left the game" are one test: the rule's first two sentences collapse to
-- "is the active player still in the game?". CR 800.4j is why "there is no
-- active player" needs no separate arm -- a turn whose active player has left
-- continues with GameState.activePlayer still naming their seat, so the absence
-- of an active player IS that seat having departed.
--
-- "the next player in turn order" is read as the next seat after the ACTIVE
-- player's, because the active player's seat is the only position in the turn
-- order the rule gives to count from.
--
-- "who can become the monarch" is read as "is still in the game": no card in the
-- pool prevents a player from becoming the monarch, and the phrase exists for
-- cards that do (#178).
reassignOnDeparture :: PlayerId -> [PlayerId] -> GameState -> GameState
reassignOnDeparture leaving playing gs =
  if GameState.monarch gs /= Just leaving
    then gs
    else
      let active = GameState.activePlayer gs
          walk = case List.break (== active) (GameState.turnOrder gs) of
            (before, _ : after) -> after <> before
            (before, []) -> before
          next = List.find (\pid -> List.elem pid playing) walk
       in gs
            { GameState.monarch =
                if List.elem active playing
                  then Just active
                  else next
            }

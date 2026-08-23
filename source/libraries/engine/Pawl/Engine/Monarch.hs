module Pawl.Engine.Monarch where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.Binding (Binding)
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Optionality as Optionality
import Pawl.Types.PendingTrigger (PendingTrigger)
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Phase as Phase
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.TapState as TapState
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggerSource as TriggerSource
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone

-- A single-mode, single-effect triggered ability (the shape all monarch
-- inherent abilities take): one Mode with no targets, forced (ChooseExactly 1).
-- intervening = Nothing because CR 725.2 states neither ability with an "if"
-- clause. Stack.resolveTop's OfInherentTrigger arm applies CR 608.2a's
-- resolution recheck to any inherent ability that HAS one (CR 702.179d's does),
-- so this is a fact about rule 725.2 rather than a licence the arm relies on.
oneEffect :: TriggerCondition -> Effect.Effect Card -> TriggeredAbility Card
oneEffect cond eff =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = cond,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton eff))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }

-- CR 725.2's end step draw. Controller-scoped to the monarch, so
-- ControllersTurn plus the monarch as "you" is exactly the monarch's own end
-- step.
endStepDraw :: TriggeredAbility Card
endStepDraw =
  oneEffect
    (TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Ending EndingStep.EndStep) TurnScope.ControllersTurn))
    (Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1) Nothing))

-- CR 725.2's crown steal. Controlled by the current monarch; makes a DIFFERENT
-- player (the damager's controller) the monarch.
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
-- environment. Sourceless -- no bearer, so this is a dedicated matcher rather
-- than Event.matchesTrigger.
inherentMatch :: PlayerId -> TriggerCondition -> GameState -> GameEvent -> Maybe (Map SlotName.SlotName Binding)
inherentMatch monarch cond gs event = case (cond, event) of
  (TriggerCondition.StepBegins (StepBegins.MkStepBegins wanted scope), GameEvent.StepBegan (StepBegan.MkStepBegan began active))
    | began == wanted && scopeOk scope active -> Just Map.empty
  -- CR 725.2: bind the damaging creature under the reserved trigger-source slot
  -- so Effect.BecomeMonarch ControllerOfSource crowns THAT creature's
  -- controller.
  (TriggerCondition.CreatureDealtCombatDamageToMonarch, GameEvent.DamageDealt ev)
    | DamageEvent.kind ev == DamageKind.Combat
        && DamageEvent.target ev == Recipient.ToPlayer monarch
        && Projection.isCreatureOf (DamageEvent.source ev) gs ->
        Just (Binding.setTriggerSource (DamageEvent.source ev) Map.empty)
  _ -> Nothing
  where
    -- The monarch is the seat this scope is read against: CR 725.2 makes these
    -- inherent abilities "controlled by the player who was the monarch at the
    -- time the abilities triggered", so they are the "you" CR 109.5 would give a
    -- printed one.
    scopeOk s a = Event.turnScopeAdmits s a monarch

-- CR 725.1/725.2: the inherent triggers that fire on this batch of events, as
-- ordinary PendingTriggers whose source is TriggerSource.Sourceless -- which is
-- what lets Engine.placePendingTriggers merge them into the one batch CR 603.3b
-- orders. Empty when there is no monarch (the abilities do not exist).
--
-- Not routed through Event.gatherTriggers, for the reason inherentMatch exists:
-- these abilities have no bearer, so the scan that walks the battlefield has
-- nowhere to find them. Skipping Event.interveningHolds costs nothing because
-- CR 603.4 applies to neither ability (see oneEffect); an inherent ability that
-- does carry an "if" checks it in its own gatherer, as Pawl.Engine.Speed does.
inherentMonarchPending :: [GameEvent] -> GameState -> [PendingTrigger]
inherentMonarchPending events gs = case GameState.monarch gs of
  Nothing -> []
  Just m ->
    let matchEvent ab ev = case inherentMatch m (TriggeredAbility.condition ab) gs ev of
          Nothing -> Nothing
          Just b -> Just (PendingTrigger.MkPendingTrigger TriggerSource.Sourceless m ab b)
        forAbility ab = Maybe.mapMaybe (matchEvent ab) events
     in concatMap forAbility monarchAbilities

-- Mint a sourceless inherent trigger onto the stack: the Engine.placeOne arm
-- for EVERY TriggerSource.Sourceless entry, called once CR 603.3b has fixed the
-- batch's order. Named for the monarch only because rule 725.2 was the first
-- rulebook-stated ability to need it; CR 702.179d's speed increase rides it
-- unchanged, and nothing here reads GameState.monarch.
--
-- Single mode, no targets, so the mode is selected outright with no prompt --
-- licensed by each such rule fixing its ability's full text, not by anything
-- general about sourceless triggers. An inherent ability with a real choice in
-- it would have to prompt here. The chosen modes ride under the reserved
-- chosenModes slot, which Stack.resolveTop's OfInherentTrigger arm reads.
placeInherent :: PendingTrigger -> Game ()
placeInherent pending = do
  gs <- State.get
  let controller = PendingTrigger.controller pending
      ability = PendingTrigger.ability pending
      provided = PendingTrigger.bindings pending
      (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      modeCount = Seq.length (Modal.modes (TriggeredAbility.modal ability))
      -- take, not [0 .. modeCount - 1]: a ModeIndex counts in Natural, and
      -- Natural subtraction underflows when there are no modes at all.
      allModes = Seq.fromList (fmap ModeIndex.MkModeIndex (take modeCount [0 ..]))
      bindings = Binding.setYou controller (Map.union provided (Binding.fromChoices Map.empty Nothing allModes))
      obj =
        Object.MkObject
          { Object.owner = controller,
            Object.enteredUnder = Nothing,
            Object.source =
              Source.OfInherentTrigger
                InherentTriggerSource.MkInherentTriggerSource
                  { InherentTriggerSource.controller = controller,
                    InherentTriggerSource.ability = ability
                  },
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled controller,
            Object.bindings = bindings,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
  State.put gs2 {GameState.objects = Map.insert abilId obj (GameState.objects gs2), GameState.stack = abilId : GameState.stack gs2}

-- CR 725.1 / CR 725.3: crown a player. The ONE writer of GameState.monarch once
-- a game is under way (Pawl.Engine.Setup only ever initialises it to Nothing, and
-- CR 725.4's third sentence below un-crowns rather than crowns), so everything
-- that must happen AS a player becomes the monarch happens here: the crown moves,
-- CR 603.2 gets its event, and every CR 725 exile watch an opponent's crowning
-- discharges is marked.
--
-- A player who is ALREADY the monarch does not become the monarch: Custodi Lich's
-- ruling (Gatherer, 2016-08-23) is explicit -- "abilities that trigger whenever
-- you become the monarch trigger only if you aren't already the monarch" -- and
-- CR 725.3's "as a player becomes the monarch, the current monarch ceases to be
-- the monarch" describes a handoff between two players. So the instruction is
-- carried out (the crown is where the effect says it is) while nothing is
-- recorded and no watch is marked. Both readers of "becomes the monarch" -- the
-- exile watch and TriggerCondition.PlayerBecomesMonarch -- therefore agree by
-- construction, which is the reason this is one function rather than a write at
-- each call site.
crown :: PlayerId -> GameState -> GameState
crown pid gs =
  if GameState.monarch gs == Just pid
    then gs
    else
      let -- "An opponent" is every player other than the effect's controller. Not
          -- a two-player shortcut: in a free-for-all every other player is an
          -- opponent by construction (CR 806.1), so the opponent half of the test
          -- is just "the crowned player is not the controller". Only CR 102.3's
          -- teammates would break that, and pawl has no teams (#175).
          --
          -- When the controller has LEFT the game, CR 800.4i freezes their
          -- opponent set at departure, and the same comparison computes it: CR
          -- 725.4 guarantees the crowned player is still in the game, so a
          -- departed controller is never crowned. Nothing needs to be stored.
          mark watch =
            if MonarchWatch.controller watch == pid
              then watch
              else watch {MonarchWatch.due = True}
       in Event.recordEvent
            (GameEvent.BecameMonarch pid)
            gs
              { GameState.monarch = Just pid,
                GameState.exiledUntilMonarch = fmap mark (GameState.exiledUntilMonarch gs)
              }

-- CR 725 (Palace Jailer): return every object whose watch `crown` has marked --
-- an opponent of the entry's controller HAS BECOME the monarch. Runs in the
-- settle loop; CR 704.3 fixes "whenever a player would get priority" as the
-- coarsest moment anything can observe a condition, so deciding at the crowning
-- and moving the card at the next settle is indistinguishable from moving it at
-- the crowning.
--
-- The test is for an EVENT, not a state: a new monarch being CROWNED who is an
-- opponent, not merely an opponent currently holding the crown. Palace Jailer's
-- rulings draw that line explicitly. Which is why the decision is `crown`'s and
-- not this function's: a comparison against the monarch seen at the previous
-- settle cannot tell a crown that never moved from one that moved away and came
-- back, and no comparison against the CURRENT monarch can see a reign that began
-- and ended between two settles at all. Pawl.LibraryOrderSpec's "a crown that goes
-- to an opponent and back inside one resolution still frees the prisoner" is the
-- proof (see #208).
--
-- Departure.objectsLeaveWith drops an entry whose KEY (the exiled object) belongs
-- to a departing player, never one whose VALUE does, so the effect survives its
-- controller's departure.
returnExiledForMonarch :: Game Bool
returnExiledForMonarch = do
  gs <- State.get
  let due = Map.keys (Map.filter MonarchWatch.due (GameState.exiledUntilMonarch gs))
  if null due
    then pure False
    else do
      Monad.forM_ due $ \oid -> do
        _ <- Event.changeZoneReturning oid Zone.Battlefield
        State.modify' (\g -> g {GameState.exiledUntilMonarch = Map.delete oid (GameState.exiledUntilMonarch g)})
      pure True

-- CR 725.4: reassign the crown when the monarch leaves the game.
--
-- `playing` is the still-playing seats in SEATING order, injected by the caller
-- rather than computed here. Pawl.Engine.Departure passes the seats it has just
-- recomputed from the same post-flip state, so `Game.stillPlayingInOrder gs`
-- would give the identical answer; the injection makes the caller's snapshot
-- explicit at the one moment the answer is changing.
--
-- Deliberately NOT a call to Engine.nextStillPlaying, which walks the same
-- seating order for CR 800.4a's priority successor: that anchors on the
-- DEPARTING seat and can return it, where this anchors on the ACTIVE seat and
-- excludes it (the active player's own case is the branch below), and this
-- answers Maybe, because CR 725.4's third sentence lets the game continue with
-- no monarch. Changing either walk without checking the other risks a mismatch.
--
-- Called with `leaving` ALREADY marked departed, so "is leaving the game" and
-- "has left the game" are one test and the rule's first two sentences collapse
-- to "is the active player still in the game?". CR 800.4j is why "there is no
-- active player" needs no separate arm: GameState.activePlayer still names a
-- departed seat, so that absence IS the seat having departed. "The next player
-- in turn order" counts from the active player's seat, the only position the
-- rule gives.
--
-- "Who can become the monarch" is CR 725.4's own words, and the eligibility half
-- of it is Pawl.Engine.PlayerEffect's question (Jared Carthalion, True Heir).
-- Applied to the ACTIVE player too, though the rule's first sentence states no
-- gate: the third sentence's "if no player still in the game can become the
-- monarch" only means anything if the eligibility test covers every seat the rule
-- might crown, and CR 101.2 would stop the first sentence's crowning anyway. The
-- rejected reading is the literal one, where an ineligible active player blocks
-- sentence 1, fails sentence 2's departure condition and leaves the crown
-- unmoved -- which makes sentence 3 unreachable.
--
-- The write, the CR 725.1 event record and the exile watches are ONE step,
-- because this crowns through `crown` -- for the reason Departure.depart gives
-- for keeping this call inside itself: a crowning that records nothing is a
-- crowning CR 603.2 cannot see, and separating them lets a later caller move the
-- crown silently. A rule rather than an effect moves it here, which changes
-- nothing -- CR 725.2's stolen crown is a rule too and goes the same way.
--
-- CR 725.4's third sentence is the ONE arm that writes GameState.monarch
-- directly, and rightly: it crowns nobody, so it records no event and marks no
-- watch. "An opponent becomes the monarch" is never satisfied by there being no
-- monarch.
reassignOnDeparture :: PlayerId -> [PlayerId] -> GameState -> GameState
reassignOnDeparture leaving playing gs =
  if GameState.monarch gs /= Just leaving
    then gs
    else
      let active = GameState.activePlayer gs
          walk = case List.break (== active) (GameState.turnOrder gs) of
            (before, _ : after) -> after <> before
            (before, []) -> before
          eligible pid = List.elem pid playing && not (PlayerEffect.prohibitsBecomingMonarch pid gs)
          next = List.find eligible walk
          crowned =
            if eligible active
              then Just active
              else next
       in case crowned of
            Nothing -> gs {GameState.monarch = Nothing}
            Just pid -> crown pid gs

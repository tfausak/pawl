-- | CR 726: the initiative -- the designation, its three inherent triggered
-- abilities (CR 726.2) and the hand-off when its holder leaves the game (CR
-- 726.4).
--
-- Pawl.Engine.Monarch's sibling one rule over, and built to the same plan: a
-- game-wide player designation on GameState (CR 726.1/726.3, as CR 725.1/725.3),
-- abilities minted here because the rulebook rather than a card prints them, and
-- a single writer through which every road to the designation runs.
--
-- Three places where rule 726 differs from rule 725, each load-bearing:
--
--   * CR 726.5 makes taking the initiative you already have a real event -- it
--     re-triggers the last ability in CR 726.2 -- where Monarch.crown returns
--     early for the player already wearing the crown.
--   * CR 726.2's third ability is controlled by the player who TOOK it, which is
--     the player who has the initiative at that moment; the other two are
--     controlled by the current holder.
--   * CR 726.4 has no counterpart to CR 725.4's third sentence, no rule 726
--     eligibility gate existing for it to be about.
--
-- Nothing here asks which EFFECT anything came from: casing on rule 726 is
-- casing on the rulebook, as Pawl.Engine.Dungeon's haddock puts it.
module Pawl.Engine.Initiative where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Event.Match as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.BeginningStep as BeginningStep
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.InitiativeTarget as InitiativeTarget
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Optionality as Optionality
import Pawl.Types.PendingTrigger (PendingTrigger)
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Phase as Phase
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.Subtype as Subtype
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggerSource as TriggerSource
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope

-- A single-mode, single-effect triggered ability, the shape all three of CR
-- 726.2's abilities take: one Mode with no targets, forced (ChooseExactly 1).
-- intervening = Nothing because rule 726.2 prints no "if" clause on any of the
-- three; their full text is quoted in the rule and holds no other sentence.
oneEffect :: TriggerCondition -> Effect.Effect Card (GrantedAbility.GrantedAbility Card) -> TriggeredAbility Card (GrantedAbility.GrantedAbility Card)
oneEffect cond eff =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = cond,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton eff))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }

-- CR 726.2 / 701.49d: "ventures into Undercity" -- a venture indicating the
-- dungeon type Undercity (CR 205.3p), which Pawl.Engine.Dungeon.enterable reads
-- and which Undercity's own "you can't enter this dungeon unless you 'venture
-- into Undercity'" requires.
ventureIntoUndercity :: Effect.Effect Card (GrantedAbility.GrantedAbility Card)
ventureIntoUndercity = Effect.Venture (Just Subtype.Undercity)

-- CR 726.2, first: "at the beginning of the upkeep of the player who has the
-- initiative, that player ventures into Undercity". Controller-scoped to the
-- holder, so ControllersTurn plus the holder as "you" is exactly the holder's own
-- upkeep.
upkeepVenture :: TriggeredAbility Card (GrantedAbility.GrantedAbility Card)
upkeepVenture =
  oneEffect
    (TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn))
    ventureIntoUndercity

-- CR 726.2, second: "whenever one or more creatures a player controls deal
-- combat damage to the player who has the initiative, the controller of those
-- creatures takes the initiative". Controlled by the current holder; hands the
-- designation to a DIFFERENT player.
combatHandoff :: TriggeredAbility Card (GrantedAbility.GrantedAbility Card)
combatHandoff =
  oneEffect
    TriggerCondition.CreaturesDealtCombatDamageToInitiative
    (Effect.TakeTheInitiative InitiativeTarget.ControllerOfSource)

-- CR 726.2, third: "whenever a player takes the initiative, that player ventures
-- into Undercity". CR 726.5 is this ability's rule -- it fires on a re-take too.
takeVenture :: TriggeredAbility Card (GrantedAbility.GrantedAbility Card)
takeVenture = oneEffect TriggerCondition.PlayerTookInitiative ventureIntoUndercity

-- The initiative's inherent abilities, present only while a player has it.
initiativeAbilities :: [TriggeredAbility Card (GrantedAbility.GrantedAbility Card)]
initiativeAbilities = [upkeepVenture, combatHandoff, takeVenture]

-- CR 726.2, first ability: does this event begin the holder's upkeep? Read off
-- `upkeepVenture`'s own condition rather than restated, so the ability's text and
-- the match cannot drift. Sourceless -- there is no bearer, so this is a
-- dedicated matcher rather than Event.matchesTrigger, Monarch.inherentMatch's
-- reason.
--
-- The holder is the seat the TurnScope is read against: CR 726.2 makes these
-- abilities "controlled by the player who had the initiative at the time the
-- abilities triggered", so they are the "you" CR 109.5 would give a printed one.
beginsHoldersUpkeep :: PlayerId -> GameState -> LoggedEvent.LoggedEvent -> Bool
beginsHoldersUpkeep holder gs logged = case (TriggeredAbility.condition upkeepVenture, LoggedEvent.event logged) of
  (TriggerCondition.StepBegins (StepBegins.MkStepBegins wanted scope), GameEvent.StepBegan (StepBegan.MkStepBegan began active)) ->
    began == wanted && Event.turnScopeAdmits (Game.teams gs) scope active holder
  _ -> False

-- CR 726.2: the inherent triggers that fire on this batch of events, as ordinary
-- PendingTriggers whose source is TriggerSource.Sourceless -- which is what lets
-- Engine.placePendingTriggers merge them into the one batch CR 603.3b orders.
--
-- Not routed through Event.gatherTriggers, for Monarch.inherentMonarchPending's
-- reason: these abilities have no bearer, so the scan that walks the battlefield
-- has nowhere to find them. Skipping Event.interveningHolds costs nothing because
-- CR 603.4 applies to none of the three (see `oneEffect`).
--
-- The first two abilities exist only while a player has the initiative and are
-- controlled by that player. The THIRD is gathered whatever the board holds now
-- and is controlled by the player its event names: CR 726.2's "the player who had
-- the initiative at the time the abilities triggered" is, for an ability that
-- triggers ON the take, the taker -- and reading GameState.initiative for it
-- would name the wrong player once two takes land in one batch.
--
-- CR 726.2's second ability is a BATCH reading -- "whenever one or more creatures
-- a player controls deal combat damage" -- so the damage events are grouped by
-- the damaging creature's controller and each controller gets ONE trigger, in the
-- order their first damaging creature appears; the first creature stands for the
-- batch, since Effect.TakeTheInitiative reads only its controller.
-- Pawl.Engine.Damage.dealWave brackets a CR 510.2 combat damage step as one wave
-- and each damage step ends in a settle, so the batch scanned here is that step's
-- damage: a first-strike step and the regular step are separate batches and
-- rightly trigger twice.
--
-- That is where this parts company with Monarch.inherentMonarchPending, and the
-- divergence is the two RULES' own: CR 725.2 prints "whenever A CREATURE deals
-- combat damage to the monarch, ITS controller becomes the monarch", one trigger
-- per damaging creature, where CR 726.2 prints "whenever ONE OR MORE creatures a
-- player controls deal combat damage", one per controller. Neither reading is a
-- generalisation of the other and neither module may borrow the other's.
--
-- The holder is read LIVE, where the damagers come off CR 603.10's sample, for
-- Monarch.inherentMonarchPending's reason: only CR 726.4's departure hand-off
-- can move the designation inside one settle, and the hand-off it would then
-- have gathered belongs to the departed holder, whom CR 800.4d keeps off the
-- stack. Proved by Pawl.InitiativeSpec's "CR 726.4/800.4d lethal combat damage
-- to the holder hands the initiative to the active player, not the damager's
-- controller".
inherentPending :: [LoggedEvent.LoggedEvent] -> GameState -> [PendingTrigger]
inherentPending events gs = held <> takes
  where
    held = case GameState.initiative gs of
      Nothing -> []
      Just holder -> upkeeps holder <> handoffs holder
    sourceless controller ability bindings = PendingTrigger.MkPendingTrigger TriggerSource.Sourceless controller ability bindings Nothing
    upkeeps holder = fmap (\_ -> sourceless holder upkeepVenture Map.empty) (filter (beginsHoldersUpkeep holder gs) events)
    -- Each pair is a damaging creature and who controlled it AS THE DAMAGE WAS
    -- DEALT (Event.combatDamagerAgainst, which is also what screens the event
    -- shape), so a creature the same step's state-based actions have already
    -- destroyed is still one of "those creatures".
    handoffs holder =
      let damagers = Maybe.mapMaybe (Event.combatDamagerAgainst holder gs) events
          sameController a b = snd a == snd b
       in fmap (\(oid, _) -> sourceless holder combatHandoff (Binding.setTriggerSource oid Map.empty)) (List.nubBy sameController damagers)
    -- A PARTIAL case with a wildcard, Speed.inherentPending's posture: this
    -- matcher answers about one event shape, and a new GameEvent constructor is
    -- not an event rule 726.2 names.
    takes = Maybe.mapMaybe taker events
    taker logged = case LoggedEvent.event logged of
      GameEvent.TookInitiative pid -> Just (sourceless pid takeVenture Map.empty)
      _ -> Nothing

-- | CR 726.1 / 726.3 / 726.5: a player takes the initiative. The ONE writer of
-- GameState.initiative once a game is under way (Pawl.Engine.Setup only ever
-- initialises it to Nothing), so everything that must happen AS a player takes it
-- happens here: the designation moves and CR 603.2 gets its event.
--
-- Recorded even when the taker ALREADY has the initiative, where Monarch.crown
-- records nothing for a re-crowning: CR 726.5 says in as many words that this
-- "causes the last triggered ability in 726.2 to trigger but does not create a
-- second initiative designation". The Just write is that second sentence -- one
-- designation, moved or left where it is -- and the event is the first.
takeInitiative :: PlayerId -> GameState -> GameState
takeInitiative pid gs = Event.recordEvent (GameEvent.TookInitiative pid) gs {GameState.initiative = Just pid}

-- | CR 726.4: hand the initiative on when its holder leaves the game.
--
-- `playing` is the still-playing seats in SEATING order, injected by the caller
-- for Monarch.reassignOnDeparture's reason, and called with `leaving` ALREADY
-- marked departed, so "is leaving the game" and "has left the game" are one test.
-- CR 800.4j is why "there is no active player" needs no separate arm:
-- GameState.activePlayer still names a departed seat, so that absence IS the seat
-- having departed.
--
-- No eligibility gate, where CR 725.4 has one: rule 726 states no "can't take the
-- initiative" effect for one to read, and Pawl.Types.PlayerEffect has no such arm
-- to consult.
--
-- Nothing left to take it is unreachable rather than a rule: CR 104.2a ends the
-- game as soon as one player is left, so no departure empties the seats. Answered
-- Nothing rather than left naming the departed holder, because a designation held
-- by a player who has left the game is a state no rule describes.
reassignOnDeparture :: PlayerId -> [PlayerId] -> GameState -> GameState
reassignOnDeparture leaving playing gs =
  if GameState.initiative gs /= Just leaving
    then gs
    else
      let active = GameState.activePlayer gs
          walk = case List.break (== active) (GameState.turnOrder gs) of
            (before, _ : after) -> after <> before
            (before, []) -> before
          eligible pid = List.elem pid playing
          taker =
            if eligible active
              then Just active
              else List.find eligible walk
       in case taker of
            Nothing -> gs {GameState.initiative = Nothing}
            Just pid -> takeInitiative pid gs

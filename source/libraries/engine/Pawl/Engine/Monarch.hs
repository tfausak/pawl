module Pawl.Engine.Monarch where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mint as Mint
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Phase as Phase
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.ReturnEnding as ReturnEnding
import qualified Pawl.Types.ReturnWatch as ReturnWatch
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TurnScope as TurnScope

-- CR 725.2's end step draw. Controller-scoped to the monarch, so
-- ControllersTurn plus the monarch as "you" is exactly the monarch's own end
-- step.
endStepDraw :: TriggeredAbility Card (GrantedAbility.GrantedAbility Card)
endStepDraw =
  Mint.trigger
    (TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Ending EndingStep.EndStep) Nothing TurnScope.ControllersTurn))
    (Seq.singleton (Effect.Draw (Draw.MkDraw (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1) Nothing)))

-- CR 725.2's crown steal. Controlled by the current monarch; makes a DIFFERENT
-- player (the damager's controller) the monarch.
crownSteal :: TriggeredAbility Card (GrantedAbility.GrantedAbility Card)
crownSteal =
  Mint.trigger
    TriggerCondition.CreatureDealtCombatDamageToMonarch
    (Seq.singleton (Effect.BecomeMonarch MonarchTarget.ControllerOfSource))

-- CR 725.2: the monarch's inherent abilities, each paired with its controller,
-- for Event.Trigger.inherentTriggers. Present only while there is a monarch,
-- and controlled by "the player who was the monarch at the time the abilities
-- triggered", which is the monarch read here.
--
-- The monarch is read LIVE, where the crown steal's damager comes off CR
-- 603.10's sample, and that is sound: inside one settle only CR 725.4's
-- departure hand-off can move the crown between the damage and the gather, and
-- the steal it would then have gathered is controlled by the departed monarch,
-- whom CR 800.4d keeps off the stack -- so the active player holds the crown
-- either way. Proved by Pawl.EventTriggerSpec's "CR 725.4/800.4d lethal combat
-- damage to the monarch crowns the active player, not the damager's
-- controller".
abilities :: GameState -> [(PlayerId, TriggeredAbility Card (GrantedAbility.GrantedAbility Card))]
abilities gs = case GameState.monarch gs of
  Nothing -> []
  Just monarch -> [(monarch, endStepDraw), (monarch, crownSteal)]

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
--
-- CR 725 (Palace Jailer): the return of an object whose watch this has
-- marked is Pawl.Engine.MoveDuration.returnDue's, CR 610.3's second one-shot
-- effect. The test is for an EVENT, not a state: a new monarch being CROWNED who
-- is an opponent, not merely an opponent currently holding the crown. Palace
-- Jailer's rulings draw that line explicitly. Which is why the decision is
-- made here: a comparison against the monarch seen at the previous settle cannot
-- tell a crown that never moved from one that moved away and came back, and no
-- comparison against the CURRENT monarch can see a reign that began and ended
-- between two settles at all. Pawl.LibraryOrderSpec's "a crown that goes to an
-- opponent and back inside one resolution still frees the prisoner" is the
-- proof (see #208).
crown :: PlayerId -> GameState -> GameState
crown pid gs =
  if GameState.monarch gs == Just pid
    then gs
    else
      let -- "An opponent" is CR 102.3's: every player not on the entry
          -- controller's team, which in a free-for-all (CR 806.1) is every other
          -- player. Game.areOpponents is the predicate, so a teammate crowned in
          -- a Team vs. Team game does not discharge the watch.
          --
          -- When the controller has LEFT the game, CR 800.4i freezes their
          -- opponent set at departure, and the same predicate computes it: CR
          -- 725.4 guarantees the crowned player is still in the game, so a
          -- departed controller is never crowned, and CR 800.2's teams are
          -- settled before the game begins. Nothing needs to be stored.
          mark watch = case ReturnWatch.ending watch of
            ReturnEnding.OpponentCrowned crowned
              | Game.areOpponents gs (MonarchWatch.controller crowned) pid && Maybe.isNothing (MonarchWatch.due crowned) ->
                  watch {ReturnWatch.ending = ReturnEnding.OpponentCrowned crowned {MonarchWatch.due = Just (GameState.nextEventGroup gs)}}
            _ -> watch
       in Event.recordEvent
            (GameEvent.BecameMonarch pid)
            gs
              { GameState.monarch = Just pid,
                GameState.movedUntil = fmap mark (GameState.movedUntil gs)
              }

-- CR 725.4: reassign the crown when the monarch leaves the game. Who takes it is
-- Game.heirOnDeparture's question, shared with CR 726.4.
--
-- Called with `leaving` ALREADY marked departed, so "is leaving the game" and
-- "has left the game" are one test and the rule's first two sentences collapse
-- to "is an active player still in the game?".
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
-- because this crowns through `crown` -- for the reason
-- Departure.departTogether gives for keeping this call inside itself: a
-- crowning that records nothing is a crowning CR 603.2 cannot see, and
-- separating them lets a later caller move the crown silently. A rule rather than an effect moves it here, which changes
-- nothing -- CR 725.2's stolen crown is a rule too and goes the same way.
--
-- CR 725.4's third sentence is the ONE arm that writes GameState.monarch
-- directly, and rightly: it crowns nobody, so it records no event and marks no
-- watch. "An opponent becomes the monarch" is never satisfied by there being no
-- monarch.
reassignOnDeparture :: PlayerId -> Game ()
reassignOnDeparture leaving = do
  held <- State.gets GameState.monarch
  Monad.when (held == Just leaving) $ do
    gs <- State.get
    let eligible pid = List.elem pid (Game.stillPlaying gs) && not (PlayerEffect.prohibitsBecomingMonarch pid gs)
    crowned <- Game.heirOnDeparture eligible
    State.modify' $ \g -> case crowned of
      Nothing -> g {GameState.monarch = Nothing}
      Just pid -> crown pid g

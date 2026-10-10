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

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mint as Mint
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Effect as Effect
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.InitiativeTarget as InitiativeTarget
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import Pawl.Types.TriggeredAbility (TriggeredAbility)

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
  Mint.trigger
    Mint.yourUpkeep
    (Seq.singleton ventureIntoUndercity)

-- CR 726.2, second: "whenever one or more creatures a player controls deal
-- combat damage to the player who has the initiative, the controller of those
-- creatures takes the initiative". Controlled by the current holder; hands the
-- designation to a DIFFERENT player.
combatHandoff :: TriggeredAbility Card (GrantedAbility.GrantedAbility Card)
combatHandoff =
  Mint.trigger
    TriggerCondition.CreaturesDealtCombatDamageToInitiative
    (Seq.singleton (Effect.TakeTheInitiative InitiativeTarget.ControllerOfSource))

-- CR 726.2, third: "whenever a player takes the initiative, that player ventures
-- into Undercity". CR 726.5 is this ability's rule -- it fires on a re-take too.
takeVenture :: TriggeredAbility Card (GrantedAbility.GrantedAbility Card)
takeVenture = Mint.trigger TriggerCondition.PlayerTookInitiative (Seq.singleton ventureIntoUndercity)

-- CR 726.2: the initiative's inherent abilities, each paired with its
-- controller, for Event.Trigger.inherentTriggers.
--
-- The first two exist only while a player has the initiative and are controlled
-- by that player. The THIRD is offered to every seat and matches the take whose
-- taker is its controller: CR 726.2's "the player who had the initiative at the
-- time the abilities triggered" is, for an ability that triggers ON the take,
-- the taker -- and reading GameState.initiative for it would name the wrong
-- player once two takes land in one batch.
--
-- CR 726.2's second ability is a BATCH reading -- "whenever one or more
-- creatures a player controls deal combat damage" -- where CR 725.2's crown
-- steal is one trigger per creature; Event.Trigger.batchScoped and
-- batchPartition carry the difference. Pawl.Engine.Damage.dealWave brackets
-- each CR 510.2 damage step as one group, so a first-strike step and the
-- regular step rightly trigger twice.
--
-- The holder is read LIVE, where the damagers come off CR 603.10's sample, for
-- Monarch.abilities' reason: only CR 726.4's departure hand-off can move the
-- designation inside one settle, and the hand-off it would then have gathered
-- belongs to the departed holder, whom CR 800.4d keeps off the stack. Proved by
-- Pawl.InitiativeSpec's "CR 726.4/800.4d lethal combat damage to the holder
-- hands the initiative to the active player, not the damager's controller".
abilities :: GameState -> [(PlayerId, TriggeredAbility Card (GrantedAbility.GrantedAbility Card))]
abilities gs =
  let held = case GameState.initiative gs of
        Nothing -> []
        Just holder -> [(holder, upkeepVenture), (holder, combatHandoff)]
   in held <> fmap (\pid -> (pid, takeVenture)) (Map.keys (GameState.players gs))

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

-- | CR 726.4: hand the initiative on when its holder leaves the game. Who takes
-- it is Game.heirOnDeparture's question, shared with CR 725.4.
--
-- No eligibility gate beyond still playing, where CR 725.4 has one: rule 726
-- states no "can't take the initiative" effect for one to read, and
-- Pawl.Types.PlayerEffect has no such arm to consult.
--
-- Nothing left to take it is unreachable rather than a rule: CR 104.2a ends the
-- game as soon as one player is left, so no departure empties the seats. Answered
-- Nothing rather than left naming the departed holder, because a designation held
-- by a player who has left the game is a state no rule describes.
reassignOnDeparture :: PlayerId -> Game ()
reassignOnDeparture leaving = do
  held <- State.gets GameState.initiative
  Monad.when (held == Just leaving) $ do
    playing <- State.gets Game.stillPlaying
    taker <- Game.heirOnDeparture (`List.elem` playing)
    State.modify' $ \gs -> case taker of
      Nothing -> gs {GameState.initiative = Nothing}
      Just pid -> takeInitiative pid gs

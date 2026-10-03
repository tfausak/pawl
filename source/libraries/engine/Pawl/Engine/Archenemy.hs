-- | CR 904, the Archenemy variant: the scheme deck, CR 701.32's set in motion
-- (CR 703.4e's turn-based action), CR 701.33's abandon, and CR 704.6e's
-- state-based action for schemes, which CR 205.4h's ongoing schemes are exempt
-- from.
--
-- No GameSettings field, Pawl.Engine.Vanguard's posture: every rule here is
-- stated of the archenemy's scheme deck or a scheme card, so a player who
-- brought a scheme deck is an archenemy (isArchenemy). CR 904.12b's Supervillain
-- Rumble, where every player is one, is that same reading.
--
-- Not implemented: CR 904.13d's scheme deck construction, deck legality being
-- unchecked (#4458).
module Pawl.Engine.Archenemy where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Event.Trigger as Trigger
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Scheme as Scheme
import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.Deck as Deck
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameSettings as GameSettings
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.SchemeSetInMotion as SchemeSetInMotion
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TeamId as TeamId
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource

-- | CR 904.3: this player's scheme deck, top first.
deckOf :: PlayerId -> GameState -> [ObjectId]
deckOf pid gs = foldMap Foldable.toList (Map.lookup pid (GameState.schemeDecks gs))

-- | CR 904.2a / 904.12b: is this player an archenemy? They brought a scheme
-- deck.
isArchenemy :: PlayerId -> GameState -> Bool
isArchenemy pid gs = Map.member pid (GameState.schemeDecks gs)

-- | CR 904.2a: the one player of this matchup bringing a scheme deck, when
-- exactly one does. Nothing for CR 904.12's Supervillain Rumble, whose every
-- player is an archenemy and which keeps a random starting player (CR
-- 904.12c), nor for a game with no archenemy.
sole :: [(PlayerId, Deck.Deck)] -> Maybe PlayerId
sole matchup = case [pid | (pid, deck) <- matchup, not (Map.null (Deck.schemes deck))] of
  [pid] -> Just pid
  _ -> Nothing

-- | CR 904.2: an Archenemy game's settings -- two teams, this archenemy alone
-- on one (CR 904.2a) and every other seat on the other (CR 904.2b), with the
-- attack multiple players and shared team turns options (CR 805.1). No choice
-- is made: CR 904.13's Commander option keeps the same two teams.
setUp :: PlayerId -> GameState -> GameState
setUp archenemy gs =
  let teamOf pid = TeamId.MkTeamId (if pid == archenemy then 0 else 1)
   in gs
        { GameState.settings =
            (GameState.settings gs)
              { GameSettings.teams = Teams.MkTeams (Map.fromList [(pid, teamOf pid) | pid <- GameState.turnOrder gs]),
                GameSettings.sharedTeamTurns = True,
                GameSettings.attackOption = Just AttackOption.MultiplePlayers
              }
        }

-- | CR 904.5: the archenemy starts at 40 life where every other player starts
-- at 20, so 20 more.
lifeBonus :: Integer
lifeBonus = 20

-- | CR 314.4: the face-up scheme cards.
faceUp :: GameState -> [ObjectId]
faceUp gs = filter (`Scheme.isScheme` gs) (Set.toAscList (GameState.command gs))

-- | CR 103.3a: shuffle this player's scheme deck.
shuffleSchemeDeck :: PlayerId -> Game ()
shuffleSchemeDeck pid = do
  gs <- State.get
  let ids = deckOf pid gs
  Monad.unless (null ids) $ do
    answer <- Game.ask (Prompt.Shuffle ids)
    let shuffled = Game.honourShuffle ids answer
    State.modify' (\g -> g {GameState.schemeDecks = Map.insert pid (Seq.fromList shuffled) (GameState.schemeDecks g)})

-- | CR 701.32b / 904.9: move the top card of this archenemy's scheme deck off it
-- and turn it face up, which is joining GameState.command. The event is what
-- CR 904.9's "When you set this scheme in motion" triggers on. A fresh
-- timestamp, Pawl.Engine.Planechase.turnUpTop's reason.
--
-- CR 101.2: nothing happens while an effect says schemes can't be set in motion
-- (All in Good Time, PlayerEffect.schemesCantBeSetInMotion).
setInMotion :: PlayerId -> Game ()
setInMotion pid = do
  gs <- State.get
  case deckOf pid gs of
    _ | PlayerEffect.schemesCantBeSetInMotion gs -> pure ()
    [] -> pure ()
    top : rest -> do
      ts <- State.state Game.freshTimestamp
      State.modify' $ \g ->
        Event.recordEvent
          (GameEvent.SchemeSetInMotion (SchemeSetInMotion.MkSchemeSetInMotion pid top))
          g
            { GameState.schemeDecks = Map.insert pid (Seq.fromList rest) (GameState.schemeDecks g),
              GameState.command = Set.insert top (GameState.command g),
              GameState.objects = Map.adjust (\o -> o {Object.timestamp = ts}) top (GameState.objects g)
            }

-- | CR 701.33a: may this scheme be abandoned? Only a face-up ongoing one.
canAbandon :: ObjectId -> GameState -> Bool
canAbandon oid gs = Set.member oid (GameState.command gs) && Scheme.isScheme oid gs && Scheme.isOngoing oid gs

-- | CR 701.33b: turn this scheme face down and put it on the bottom of its
-- owner's scheme deck. Nothing for one CR 701.33a does not allow.
abandon :: ObjectId -> Game ()
abandon oid = do
  gs <- State.get
  Monad.when (canAbandon oid gs) (State.modify' (toBottom oid))

-- | CR 701.33b / 704.6e: a face-up scheme goes to the bottom of its owner's
-- scheme deck, face down. The same object: unlike a plane (CR 311.6), no rule
-- makes a scheme turned face down a new one.
toBottom :: ObjectId -> GameState -> GameState
toBottom oid gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    gs
      { GameState.command = Set.delete oid (GameState.command gs),
        GameState.schemeDecks = Map.insertWith (flip (Seq.><)) (Object.owner obj) (Seq.singleton oid) (GameState.schemeDecks gs)
      }

-- | CR 704.6e / 314.6: the face-up non-ongoing schemes to put on the bottom of
-- their decks, when no triggered ability of any scheme is on the stack or
-- waiting to be put on it. CR 205.4h exempts an ongoing scheme.
--
-- "Waiting" is asked of the trigger gather itself, over the events this settle
-- has not scanned, Pawl.Engine.Dungeon.finished's reason: this pass runs before
-- Engine.placePendingTriggers, so an ability triggered on such an event is in
-- no stack and no batch yet.
abandonedBySba :: GameState -> [ObjectId]
abandonedBySba gs =
  let spent = filter (\oid -> not (Scheme.isOngoing oid gs)) (faceUp gs)
      schemeOnStack = any fromScheme (GameState.stack gs)
      fromScheme stacked = case fmap Object.source (Game.lookupObject stacked gs) of
        Just (Source.OfTrigger triggered) -> Scheme.isScheme (TriggeredAbilitySource.source triggered) gs
        _ -> False
      waiting = any pendingFromScheme (Trigger.eventTriggers (Event.unscannedGrouped gs) gs)
      pendingFromScheme pending = case PendingTrigger.source pending of
        TriggerSource.OfObject oid -> Scheme.isScheme oid gs
        TriggerSource.Sourceless -> False
   in if null spent || schemeOnStack || waiting then [] else spent

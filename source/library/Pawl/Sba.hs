module Pawl.Sba where

import Control.Applicative ((<|>))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.Departure as Departure
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.Zone as Zone

stillPlaying :: GameState -> [PlayerId]
stillPlaying gs =
  let isPlaying entry = Player.status (snd entry) == Status.Playing
   in map fst (filter isPlaying (Map.toList (GameState.players gs)))

-- CR 704.5a (life <= 0) and CR 704.5b (drawing from an empty library).
losesNow :: GameState -> PlayerId -> Bool
losesNow gs pid = case Map.lookup pid (GameState.players gs) of
  Nothing -> False
  Just player ->
    Player.status player == Status.Playing
      && (Player.life player <= 0 || Set.member pid (GameState.drewFromEmpty gs))

depart :: PlayerId -> GameState -> GameState
depart pid gs =
  let lose p = p {Player.status = Status.Departed Departure.Lost}
   in gs {GameState.players = Map.adjust lose pid (GameState.players gs)}

-- CR 704.5h: a creature with toughness > 0 dealt damage by a deathtouch source
-- since the last SBA check is destroyed. "Deathtouch source" is read from the
-- event's deal-time bit (CR 702.2e last-known information), NOT re-derived now --
-- so a source that lost deathtouch (Humility) or left after dealing damage is
-- still judged by what it was. See the M3b spec, section 4.
woundedByDeathtouch :: GameState -> ObjectId -> Bool
woundedByDeathtouch gs oid =
  let hits ev =
        DamageEvent.target ev == Recipient.ToCreature oid
          && DamageEvent.amount ev > 0
          && DamageEvent.dealtByDeathtouch ev
   in any hits (GameState.damageEvents gs)

-- CR 704.5f (toughness 0 or less), CR 704.5g (damage marked >= toughness), and
-- CR 704.5h (wounded by a deathtouch source).
--
-- The isCreature guard is not redundant. Only creatures have printed toughness
-- today, so toughnessOf already implies it -- but a Vehicle (CR 301.7) has P/T
-- while not being a creature, and 704.5f/g must not touch it. Ask the
-- classification, never the identity.
-- Takes the object's already-projected characteristics (checkStateBasedActions
-- projects the whole board once, per CR 704.4 simultaneity, rather than
-- re-projecting per object).
creatureDies :: GameState -> PC.ProjectedCharacteristics -> ObjectId -> Bool
creatureDies gs pc oid =
  let isCreature = Set.member CardType.Creature (PC.cardTypes pc)
   in isCreature && case PC.toughness pc of
        -- An unevaluable toughness means NO state-based action, not a crash.
        -- Unreachable in M1b (every toughness is a Literal); reachable at M3.
        Nothing -> False
        Just toughness ->
          -- CR 704.5f: toughness 0 or less.
          (toughness <= 0)
            -- CR 704.5g: damage marked is lethal.
            || ( case Game.lookupObject oid gs of
                   Nothing -> False
                   Just obj -> toInteger (Object.damage obj) >= toughness
               )
            -- CR 704.5h: wounded by a deathtouch source (toughness > 0 already,
            -- since 704.5f handled <= 0).
            || woundedByDeathtouch gs oid

-- CR 704.3 says to repeat until no state-based action is performed. One pass is
-- enough in M1b: a creature dying cannot cause another SBA, because nothing
-- gains or loses life when a creature dies. Revisit when it can.
checkStateBasedActions :: GameState -> GameState
checkStateBasedActions = snd . performStateBasedActions

-- One SBA pass, also reporting whether any state-based action was PERFORMED (a
-- creature buried or a player departed). CR 704.4: the caller repeats the check
-- while that flag is True. The flag lets the CR 117.5 settle loop (Engine) decide
-- whether to repeat WITHOUT a deep GameState comparison -- the common case (no SBA
-- fires) then costs one projection, not a full-state equality traversal.
performStateBasedActions :: GameState -> (Bool, GameState)
performStateBasedActions gs =
  let -- CR 704.5f/g are checked against the state BEFORE any of them apply: SBAs
      -- are simultaneous. Project the whole board once (one gather) and judge each
      -- object against it, rather than re-projecting per object.
      pcs = Projection.projectAll gs
      dies oid = case Map.lookup oid pcs of
        Nothing -> False
        Just pc -> creatureDies gs pc oid
      dying = filter dies (Set.toList (GameState.battlefield gs))
      bury g oid = Event.changeZone oid Zone.Graveyard g
      buried = List.foldl' bury gs dying
      leaving = filter (losesNow buried) (stillPlaying buried)
      departed = foldr depart buried leaving
      remaining = stillPlaying departed
      outcome = case remaining of
        [winner] -> Just (Result.Won winner)
        [] -> if null leaving then Nothing else Just Result.Drawn
        _ -> Nothing
      -- CR 704.5h's window is "since the last SBA check", so the events are
      -- drained here: dying/buried were computed from the pre-drain gs, so every
      -- 704.5h victim is found before its event is discarded.
      drained = departed {GameState.damageEvents = []}
      -- A state-based action was performed iff a creature was buried or a player
      -- left. Draining damageEvents alone is not an SBA and does not force a repeat.
      acted = not (null dying) || not (null leaving)
   in (acted, drained {GameState.result = outcome <|> GameState.result drained})

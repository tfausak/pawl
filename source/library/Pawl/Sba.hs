module Pawl.Sba where

import Control.Applicative ((<|>))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.Departure as Departure
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Source as Source
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

-- CR 704.5f: toughness 0 or less -- a put-into-graveyard, NOT a destruction, so
-- ungated by indestructible and NOT saved by regeneration (CR 701.19a).
--
-- The isCreature guard is not redundant. Only creatures have printed toughness
-- today, so toughnessOf already implies it -- but a Vehicle (CR 301.7) has P/T
-- while not being a creature, and 704.5f/g must not touch it. Ask the
-- classification, never the identity.
-- Takes the object's already-projected characteristics (checkStateBasedActions
-- projects the whole board once, per CR 704.4 simultaneity, rather than
-- re-projecting per object).
zeroToughness :: PC.ProjectedCharacteristics -> Bool
zeroToughness pc =
  Set.member CardType.Creature (PC.cardTypes pc)
    && case PC.toughness pc of
      Nothing -> False
      Just t -> t <= 0

-- CR 704.5g/h: a creature destroyed by lethal marked damage or by a deathtouch
-- source. A DESTRUCTION -- indestructible-gated (CR 700.4) and regeneration-
-- interceptable (CR 701.19a via Event.destroy). Excludes 704.5f (that is
-- zeroToughness), so toughness here is > 0.
destroyedBySba :: GameState -> PC.ProjectedCharacteristics -> ObjectId -> Bool
destroyedBySba gs pc oid =
  let isCreature = Set.member CardType.Creature (PC.cardTypes pc)
      indestructible = Set.member Keyword.Indestructible (PC.keywords pc)
   in isCreature && not indestructible && case PC.toughness pc of
        Nothing -> False
        Just toughness ->
          toughness > 0
            && ( ( case Game.lookupObject oid gs of
                     Nothing -> False
                     Just obj -> toInteger (Object.damage obj) >= toughness
                 )
                   || woundedByDeathtouch gs oid
               )

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
      classify oid = case Map.lookup oid pcs of
        Nothing -> Nothing
        Just pc
          -- CR 704.5f wins when both apply: toughness <= 0 is a put-into-graveyard.
          | zeroToughness pc -> Just False
          | destroyedBySba gs pc oid -> Just True
          | otherwise -> Nothing
      onBattlefield = Set.toList (GameState.battlefield gs)
      toGraveyard = filter (\oid -> classify oid == Just False) onBattlefield
      toDestroy = filter (\oid -> classify oid == Just True) onBattlefield
      -- CR 704.5f: a plain put-into-graveyard (regeneration cannot save it).
      buried = List.foldl' (\g oid -> Event.changeZone oid Zone.Graveyard g) gs toGraveyard
      -- CR 704.5g/h: destruction through the funnel (regeneration may replace it).
      destroyed = List.foldl' (flip Event.destroy) buried toDestroy
      leaving = filter (losesNow destroyed) (stillPlaying destroyed)
      departed = foldr depart destroyed leaving
      remaining = stillPlaying departed
      -- CR 704.5d: a token in any zone other than the battlefield ceases to exist.
      -- Computed from the post-bury state so a token that just died (now in the
      -- graveyard) or was redirected (Rest in Peace -> exile) is removed here; its
      -- move already emitted a zone-change event, so a future dies-trigger still
      -- sees it (CR 111.7's parenthetical). Keyed to "not on the battlefield", never
      -- to a specific zone, so exile is caught too.
      isVanishingToken oid = case Game.lookupObject oid departed of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfToken _ -> Object.zone obj /= Zone.Battlefield
          _ -> False
      vanishing = filter isVanishingToken (Map.keys (GameState.objects departed))
      ceaseToExist g oid = case Game.lookupObject oid g of
        Nothing -> g
        Just obj ->
          let g1 = Game.removeFromZones (Object.owner obj) oid g
           in g1 {GameState.objects = Map.delete oid (GameState.objects g1)}
      vanished = List.foldl' ceaseToExist departed vanishing
      -- CR 704.5q / 122.3: a permanent with both a +1/+1 and a -1/-1 counter has N
      -- of each removed (N = min). A counter-count edit, not a bury or departure --
      -- it feeds the `acted` flag (CR 704.4 repeats) but never re-fires once
      -- balanced. Net P/T is preserved, so it can neither cause nor prevent a
      -- death; ordering vs the bury/destroy step is immaterial.
      annihilateOne oid = case Game.lookupObject oid gs of
        Nothing -> Nothing
        Just obj ->
          let cs = Object.counters obj
              plus = Map.findWithDefault 0 CounterKind.PlusOnePlusOne cs
              minus = Map.findWithDefault 0 CounterKind.MinusOneMinusOne cs
              n = min plus minus
           in if n > 0 then Just (oid, n) else Nothing
      annihilations = Maybe.mapMaybe annihilateOne onBattlefield
      removeN n c = let c' = c - n in if c' == 0 then Nothing else Just c'
      balance g (oid, n) =
        let strip obj = obj {Object.counters = Map.update (removeN n) CounterKind.MinusOneMinusOne (Map.update (removeN n) CounterKind.PlusOnePlusOne (Object.counters obj))}
         in g {GameState.objects = Map.adjust strip oid (GameState.objects g)}
      outcome = case remaining of
        [winner] -> Just (Result.Won winner)
        [] -> if null leaving then Nothing else Just Result.Drawn
        _ -> Nothing
      -- CR 704.5h's window is "since the last SBA check", so the events are
      -- drained here: dying/buried were computed from the pre-drain gs, so every
      -- 704.5h victim is found before its event is discarded.
      drained = vanished {GameState.damageEvents = []}
      -- A state-based action was performed iff a creature was buried or destroyed
      -- (a regenerated creature still counts, which the CR 704.4 settle loop
      -- re-checks and -- because the regen healed the damage -- terminates), a
      -- player left, or a token ceased to exist.
      balanced = List.foldl' balance drained annihilations
      acted = not (null toGraveyard) || not (null toDestroy) || not (null leaving) || not (null vanishing) || not (null annihilations)
   in (acted, balanced {GameState.result = outcome <|> GameState.result balanced})

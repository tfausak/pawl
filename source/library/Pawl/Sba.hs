module Pawl.Sba where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Departure as Departure
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.Departure as Departure.Type
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.Zone as Zone

-- CR 704.5a (life <= 0), CR 704.5b (drawing from an empty library), and CR
-- 704.5c (ten or more poison counters). Two-Headed Giant's shared-poison variant
-- (CR 704.6b / 810) is out of scope (design.md §6).
losesNow :: GameState -> PlayerId -> Bool
losesNow gs pid = case Map.lookup pid (GameState.players gs) of
  Nothing -> False
  Just player ->
    Player.status player == Status.Playing
      && ( Player.life player <= 0
             || Set.member pid (GameState.drewFromEmpty gs)
             || Map.findWithDefault 0 PlayerCounterKind.Poison (Player.counters player) >= 10
         )

-- CR 704.5h: a creature with toughness > 0 dealt damage by a deathtouch source
-- since the last SBA check is destroyed. "Deathtouch source" is read from the
-- event's deal-time bit (CR 702.2e last-known information), NOT re-derived now --
-- so a source that lost deathtouch (Humility) or left after dealing damage is
-- still judged by what it was. See the M3b spec, section 4. Now read from the
-- WATERMARKED slice of the turn log, not a drained queue.
woundedByDeathtouch :: GameState -> ObjectId -> Bool
woundedByDeathtouch gs oid =
  let hits ev =
        DamageEvent.target ev == Recipient.ToCreature oid
          && DamageEvent.amount ev > 0
          && DamageEvent.dealtByDeathtouch ev
   in any hits (Event.unscannedDamage gs)

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
checkStateBasedActions :: Game ()
checkStateBasedActions = Monad.void performStateBasedActions

-- One SBA pass, also reporting whether any state-based action was PERFORMED (a
-- creature buried or a player departed). CR 704.4: the caller repeats the check
-- while that flag is True. The flag lets the CR 117.5 settle loop (Engine) decide
-- whether to repeat WITHOUT a deep GameState comparison.
--
-- Monadic since P5: CR 704.5f's put-into-graveyard and CR 704.5g's destruction
-- both go through funnels that can now raise a CR 616 replacement prompt (a
-- creature dying with two applicable death-replacements genuinely must ask its
-- controller which to apply). M3g's decider re-entrancy already permits prompting
-- from inside the settle loop.
performStateBasedActions :: Game Bool
performStateBasedActions = do
  gs <- State.get
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
      -- CR 704.5q / 122.3: a permanent with both a +1/+1 and a -1/-1 counter has N
      -- of each removed (N = min). Computed against the SAME pre-pass state as the
      -- bury/destroy classification, which is what makes the ordering immaterial:
      -- net P/T is preserved, so it can neither cause nor prevent a death.
      annihilateOne oid = case Game.lookupObject oid gs of
        Nothing -> Nothing
        Just obj ->
          let cs = Object.counters obj
              plus = Map.findWithDefault 0 CounterKind.PlusOnePlusOne cs
              minus = Map.findWithDefault 0 CounterKind.MinusOneMinusOne cs
              n = min plus minus
           in if n > 0 then Just (oid, n) else Nothing
      annihilations = Maybe.mapMaybe annihilateOne onBattlefield
      -- CR 704.5h's window is "since the last SBA check", so the watermark is the
      -- log length AS THIS PASS BEGAN: every 704.5h victim was computed from that
      -- same pre-pass state, and the Moved events this pass itself appends carry
      -- no damage. The record is never removed.
      watermark :: Natural
      watermark = fromIntegral (Seq.length (GameState.events gs))
  -- CR 704.5f: a plain put-into-graveyard (regeneration cannot save it).
  Monad.mapM_ (\oid -> Event.changeZone oid Zone.Graveyard) toGraveyard
  -- CR 704.5g/h: destruction through the funnel (regeneration may replace it).
  Monad.mapM_ Event.destroy toDestroy
  destroyed <- State.get
  let leaving = filter (losesNow destroyed) (Departure.stillPlaying destroyed)
      departed = foldr (Departure.depart Departure.Type.Lost) destroyed leaving
      -- CR 704.5d: a token in any zone other than the battlefield ceases to exist.
      -- Computed from the post-bury state so a token that just died (now in the
      -- graveyard) or was redirected (Rest in Peace -> exile) is removed here; its
      -- move already emitted a zone-change event, so a future dies-trigger still
      -- sees it (CR 111.7's parenthetical). Keyed to "not on the battlefield",
      -- never to a specific zone, so exile is caught too.
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
      removeN n c = let c' = c - n in if c' == 0 then Nothing else Just c'
      balance g (oid, n) =
        let strip obj = obj {Object.counters = Map.update (removeN n) CounterKind.MinusOneMinusOne (Map.update (removeN n) CounterKind.PlusOnePlusOne (Object.counters obj))}
         in g {GameState.objects = Map.adjust strip oid (GameState.objects g)}
      outcome = Departure.outcomeAfterLeaving leaving departed
      drained = vanished {GameState.damageScannedThrough = watermark}
      balanced = List.foldl' balance drained annihilations
      -- A state-based action was performed iff a creature was buried or destroyed
      -- (a regenerated creature still counts, which the CR 704.4 settle loop
      -- re-checks and -- because the regen healed the damage -- terminates), a
      -- player left, or a token ceased to exist.
      acted = not (null toGraveyard) || not (null toDestroy) || not (null leaving) || not (null vanishing) || not (null annihilations)
  -- This ordering lets a newly computed outcome overwrite an already-decided
  -- result, the opposite of Departure.leaveGame's ordering (#142).
  State.put balanced {GameState.result = outcome <|> GameState.result balanced}
  pure acted

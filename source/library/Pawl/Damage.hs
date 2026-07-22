module Pawl.Damage where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Combat as Combat
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Replacement as Replacement
import Pawl.Type.AttackTarget (AttackTarget)
import qualified Pawl.Type.AttackTarget as AttackTarget
import qualified Pawl.Type.Combat as Combat.Type
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import Pawl.Type.Game (Game)
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Zone as Zone

-- CR 514.2: during the cleanup step, all damage marked on permanents is removed.
--
-- Every object, not just battlefield ones: the field exists on all of them, and
-- CR 514.2 says "all damage marked on permanents (including phased-out
-- permanents)" -- there is no reason to be selective, and being selective is how
-- a stale mark survives.
removeAllDamage :: GameState -> GameState
removeAllDamage gs =
  let clear obj = obj {Object.damage = 0}
   in gs {GameState.objects = Map.map clear (GameState.objects gs)}

-- CR 510.1e / 702.19b, as a pure predicate over the whole assignment. Legal iff it
-- totals power, uses only legal recipients, and -- the trample implication -- the
-- defender got damage ONLY if every blocker is at its lethal threshold. The
-- threshold is NOT a per-blocker floor: a blocker may be under-assigned as long as
-- the defender then gets nothing. See the M2c spec, section 4.
legalAssignment :: Map.Map Recipient.Recipient Natural -> Natural -> Map.Map Recipient.Recipient Natural -> Bool
legalAssignment thresholds power answer =
  let assigned r = Map.findWithDefault 0 r answer
      totalsPower = sum (Map.elems answer) == power
      onlyLegal = all (\r -> Map.member r thresholds) (Map.keys answer)
      isDefender r = case r of
        Recipient.ToPlayer _ -> True
        Recipient.ToCreature _ -> False
        Recipient.ToObject _ -> False
      defenderAmount = sum (Map.elems (Map.filterWithKey (\r _ -> isDefender r) answer))
      blockerThresholds = Map.filterWithKey (\r _ -> not (isDefender r)) thresholds
      everyBlockerLethal = all (\(r, t) -> assigned r >= t) (Map.toList blockerThresholds)
      defenderGated = defenderAmount == 0 || everyBlockerLethal
   in totalsPower && onlyLegal && defenderGated

-- CR 702.19b / 702.2c: a blocker's lethal threshold is toughness minus marked
-- damage -- but 702.2c makes any nonzero assignment by a deathtouch source lethal,
-- so a deathtouch attacker needs only 1 (0 if the blocker is already lethal). Read
-- through the projection (Projection.hasKeyword), the same way the 704.5h SBA reads it.
blockerThreshold :: GameState -> ObjectId -> ObjectId -> Natural
blockerThreshold gs attacker blocker =
  let marked = maybe 0 Object.damage (Game.lookupObject blocker gs)
      lethal :: Natural
      lethal = case Projection.toughnessOf blocker gs of
        Nothing -> 0
        Just t -> fromInteger (max 0 (t - toInteger marked))
   in if lethal > 0 && Projection.hasKeyword Keyword.Deathtouch attacker gs
        then 1
        else lethal

-- What one attacking creature assigns, as damage events carrying the source.
-- CR 510.1a: a creature that would assign 0 or less assigns none, so events all
-- carry amount > 0.
attackerAssignment :: GameState -> (ObjectId, AttackTarget) -> Game [DamageEvent.DamageEvent]
attackerAssignment gs (attacker, target) = case Projection.powerOf attacker gs of
  Nothing -> pure []
  Just p ->
    if p <= 0
      then pure []
      else do
        let power :: Natural
            power = fromInteger p
            trample = Projection.hasKeyword Keyword.Trample attacker gs
        case Set.toList (Combat.blockersOf attacker gs) of
          -- CR 510.1b: unblocked, so it hits what it is attacking.
          [] -> case target of
            AttackTarget.OfPlayer defender ->
              pure [DamageEvent.MkDamageEvent attacker (Recipient.ToPlayer defender) power (Projection.hasKeyword Keyword.Deathtouch attacker gs) DamageKind.Combat]
          -- CR 510.1c / 702.19b: a single blocker with no trample -- or trample but
          -- no power past its threshold -- is forced: all onto the blocker. A single
          -- trample blocker WITH excess fails this guard and falls to the prompt arm.
          [blocker]
            | not trample || power <= blockerThreshold gs attacker blocker ->
                pure [DamageEvent.MkDamageEvent attacker (Recipient.ToCreature blocker) power (Projection.hasKeyword Keyword.Deathtouch attacker gs) DamageKind.Combat]
          blockers -> case Projection.controllerOf attacker gs of
            Nothing -> pure []
            -- CR 702.19b: the excess is assigned "as its controller chooses," so the
            -- chooser is the attacker's controller. Banding (CR 702.22j) inverts
            -- that -- the DEFENDING player chooses -- and is not implemented (#32).
            -- See the M2c spec, sections 4 and 8.
            Just pid -> do
              let decider = Decide.deciderFor pid gs
                  thresholdOf b = if trample then blockerThreshold gs attacker b else 0
                  blockerEntries = map (\b -> (Recipient.ToCreature b, thresholdOf b)) blockers
                  defenderEntry = case target of
                    AttackTarget.OfPlayer defender ->
                      if trample then [(Recipient.ToPlayer defender, 0 :: Natural)] else []
                  thresholds = Map.fromList (blockerEntries ++ defenderEntry)
              chosen <-
                Trans.lift
                  (Program.prompt (Prompt.AssignCombatDamage decider pid attacker thresholds power))
              -- CR 510.1e / 702.19b: reject-not-repair (NOT the CR 733 human-error
              -- rewind). An illegal answer assigns nothing. See the M2c spec, §4.
              let toEvent (recipient, n) = DamageEvent.MkDamageEvent attacker recipient n (Projection.hasKeyword Keyword.Deathtouch attacker gs) DamageKind.Combat
                  positive (_, n) = n > 0
              pure
                ( if legalAssignment thresholds power chosen
                    then map toEvent (filter positive (Map.toList chosen))
                    else []
                )

-- CR 510.1d: a blocking creature assigns its damage to the creature it blocks.
blockerAssignment :: GameState -> (ObjectId, Set.Set ObjectId) -> [DamageEvent.DamageEvent]
blockerAssignment gs (attacker, blockers) =
  let assign blocker = case Projection.powerOf blocker gs of
        Just p ->
          if p <= 0
            then []
            else [DamageEvent.MkDamageEvent blocker (Recipient.ToCreature attacker) (fromInteger p) (Projection.hasKeyword Keyword.Deathtouch blocker gs) DamageKind.Combat]
        Nothing -> []
   in concatMap assign (Set.toList blockers)

-- CR 510.2: gather all combat damage before applying any of it (simultaneity).
gatherCombatDamage :: (ObjectId -> Bool) -> Game [DamageEvent.DamageEvent]
gatherCombatDamage assigns = do
  gs <- State.get
  let combat = GameState.combat gs
      attackers = filter (assigns . fst) (Map.toList (Combat.Type.attackers combat))
      blockers = Map.toList (Map.map (Set.filter assigns) (Combat.Type.blockers combat))
  parts <- Monad.mapM (attackerAssignment gs) attackers
  let fromBlockers = concatMap (blockerAssignment gs) blockers
  pure (concat parts ++ fromBlockers)

-- CR 120.3e / 120.3a: mark damage on creatures, drain life from players -- AND
-- record each event into GameState.events. The change-and-emit funnel for
-- combat's two waves and resolving effects alike.
--
-- CR 615 / 616: EACH event in the batch runs its OWN CR 616.1 loop, and the
-- survivors are applied together. Simultaneity is preserved as a SCHEDULING
-- property; the loop's unit stays one event, uniform with the other five classes.
-- That is what CR 614.5 ("one opportunity to affect AN EVENT") and CR 615.10
-- ("applies separately to damage from other applicable events that would happen
-- at the same time") both describe.
--
-- What this shape cannot express is CR 615.7's SHARED N-damage shield -- one
-- resource allocated across several simultaneous events, with the recipient
-- choosing which it covers. No such shield exists in the pool (Fog is
-- unlimited-for-a-duration, not N-damage), so it stays card-driven (#58).
applyDamage :: [DamageEvent.DamageEvent] -> Game ()
applyDamage events = do
  survivors <- fmap Maybe.catMaybes (Monad.mapM Replacement.resolveDamage events)
  let markOne g ev = case DamageEvent.target ev of
        Recipient.ToCreature oid ->
          let hurt obj = obj {Object.damage = Object.damage obj + DamageEvent.amount ev}
           in g {GameState.objects = Map.adjust hurt oid (GameState.objects g)}
        Recipient.ToPlayer pid ->
          let drain player = player {Player.life = Player.life player - toInteger (DamageEvent.amount ev)}
           in g {GameState.players = Map.adjust drain pid (GameState.players g)}
        Recipient.ToObject _ -> g
  -- CR 608.2i: each surviving event is RECORDED, not enqueued. Sba consumes by
  -- bumping GameState.damageScannedThrough; the record survives the check.
  State.modify' (\gs -> List.foldl' (\g ev -> Event.recordEvent (GameEvent.DamageDealt ev) g) (List.foldl' markOne gs survivors) survivors)

-- Deal one combat damage step, returning True iff this was the FIRST of two --
-- i.e. a second combat damage step must be spliced (CR 510.4).
--
-- Which creatures assign is read LIVE off the projection at this boundary (spec
-- §3), never precomputed. `struckFirst` both routes the wave and records CR
-- 510.4's "had first strike or double strike as the first step began" snapshot.
-- Only creatures still on the battlefield assign ("the REMAINING attackers and
-- blockers") -- a striker killed in the first step is gone for the second.
dealCombatDamage :: Game Bool
dealCombatDamage = do
  gs <- State.get
  let combat = GameState.combat gs
      participants =
        Set.union
          (Map.keysSet (Combat.Type.attackers combat))
          (Set.unions (Map.elems (Combat.Type.blockers combat)))
      striking oid = Projection.hasKeyword Keyword.FirstStrike oid gs || Projection.hasKeyword Keyword.DoubleStrike oid gs
      strikers = Set.filter striking participants
      onBattlefield oid = case Game.lookupObject oid gs of
        Just obj -> Object.zone obj == Zone.Battlefield
        Nothing -> False
  case Combat.Type.struckFirst combat of
    Nothing
      -- CR 510.4 does not apply: no striker, so one step and everyone deals.
      | Set.null strikers -> do
          dealWave onBattlefield
          pure False
      -- CR 510.4: a striker is present. This is the first of two steps; only
      -- first strikers and double strikers deal, and a second step follows.
      | otherwise -> do
          State.modify' (\g -> g {GameState.combat = (GameState.combat g) {Combat.Type.struckFirst = Just strikers}})
          dealWave (\oid -> onBattlefield oid && Set.member oid strikers)
          pure True
    -- CR 510.4 second step: those that had neither first strike nor double strike
    -- as the first step began (not in the snapshot), plus those that currently
    -- have double strike -- and are still on the battlefield.
    Just snapshot -> do
      dealWave (\oid -> onBattlefield oid && (Set.notMember oid snapshot || Projection.hasKeyword Keyword.DoubleStrike oid gs))
      pure False

-- Gather this wave's damage under `assigns` and apply it.
dealWave :: (ObjectId -> Bool) -> Game ()
dealWave assigns = do
  assignment <- gatherCombatDamage assigns
  applyDamage assignment

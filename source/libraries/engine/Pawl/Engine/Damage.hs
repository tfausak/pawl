module Pawl.Engine.Damage where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Extra.Integer as Integer
import Pawl.Types.AttackTarget (AttackTarget)
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Zone as Zone

-- CR 514.2: during the cleanup step, all damage marked on permanents is removed.
--
-- Every object, not just battlefield ones: the field exists on all of them, and
-- CR 514.2 says "all damage marked on permanents (including phased-out
-- permanents)" -- there is no reason to be selective, and being selective is how
-- a stale mark survives.
removeAllDamage :: GameState -> GameState
removeAllDamage gs =
  let clear obj = obj {Object.damage = 0}
   in gs {GameState.objects = fmap clear (GameState.objects gs)}

-- CR 506.4: "A permanent is removed from combat if it leaves the battlefield".
-- The liveness test every combat-damage read shares, because the combat record
-- outlives the objects it names: an id in GameState.combat is a live combat
-- participant only while its object is on the battlefield. Two ways off it --
-- destroyed inside CR 510.4's two-step window, or deleted outright when its
-- owner left the game (CR 800.4a's first clause) -- so the predicate is on the
-- ZONE, not on mere existence: Event.destroy leaves the object in the graveyard.
onBattlefield :: ObjectId -> GameState -> Bool
onBattlefield oid gs = case Game.lookupObject oid gs of
  Just obj -> Object.zone obj == Zone.Battlefield
  Nothing -> False

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
        Just t -> Integer.toNaturalSaturating (t - toInteger marked)
   in if lethal > 0 && Projection.hasKeyword Keyword.Deathtouch attacker gs
        then 1
        else lethal

-- One damage event, with its deal-time riders read off the projection HERE
-- rather than re-derived when they are consumed: each is a fact about the source
-- AS THE DAMAGE IS DEALT, and the source may be gone by the time the CR 704.5h
-- SBA or a later reader asks. CR 702.2e and CR 702.90d say so outright for
-- deathtouch and infect ("its last known information is used to determine
-- whether it had" the keyword). Rule 702.164 has NO such clause, so toxic's
-- total value (CR 702.164b) is captured by analogy rather than by citation --
-- observably the same today, since applyDamage runs in the same instant, and it
-- keeps the CR 608.2i record self-contained.
--
-- Every damage the engine deals is built here -- the only other constructor call
-- in the library is Pawl.Codec's decoder, which rebuilds an event rather than
-- originating one -- so no assignment site can capture two riders and forget the
-- third.
damageEvent :: GameState -> DamageKind.DamageKind -> ObjectId -> Recipient.Recipient -> Natural -> DamageEvent.DamageEvent
damageEvent gs kind source target amount =
  DamageEvent.MkDamageEvent
    { DamageEvent.source = source,
      DamageEvent.target = target,
      DamageEvent.amount = amount,
      DamageEvent.dealtByDeathtouch = Projection.hasKeyword Keyword.Deathtouch source gs,
      DamageEvent.dealtByInfect = Projection.hasKeyword Keyword.Infect source gs,
      DamageEvent.dealtByToxic = Projection.totalToxic source gs,
      DamageEvent.kind = kind
    }

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
            power = Integer.toNaturalSaturating p
            trample = Projection.hasKeyword Keyword.Trample attacker gs
            -- CR 800.4e: "If combat damage would be assigned to a player who has
            -- left the game, that damage isn't assigned." Reachable in a
            -- multiplayer game: a defending player can concede between the
            -- declare-attackers step and the combat damage step. Read HERE, at
            -- assignment, and at both places the defender can receive damage --
            -- the unblocked/trample-through list and the CR 702.19b threshold map
            -- the prompt offers. Filtering the finished batch instead would let
            -- the attacker's controller legally assign trample excess to a player
            -- who cannot take it and then lose that damage, rather than never
            -- offering the choice.
            defenderIsPlaying = case target of
              AttackTarget.OfPlayer defender -> List.elem defender (Game.stillPlaying gs)
        -- Whether it is BLOCKED and WHO is blocking it are two questions, and the
        -- branch below asks each of the reader that answers it. CR 509.1h makes
        -- blocked-ness a status the declaration confers (Combat.isBlocked, the
        -- attacker's key in the map), which survives every blocker leaving; CR
        -- 510.1c then gives damage only to the creatures CURRENTLY blocking. The
        -- two answers come apart both ways a blocker can go: destroyed leaves it in
        -- `recorded` (the liveness filter below is the only site that screens it
        -- out, since Departure deliberately does not), regenerated takes it out of
        -- `recorded` while the key stays (Game.removeFromCombat). Reading
        -- emptiness as unblocked gets the second case wrong and lets the attacker
        -- hit the defending player. onBattlefield is the same liveness predicate
        -- dealCombatDamage uses to decide which creatures assign.
        let recorded = Combat.blockersOf attacker gs
            toDefender :: [DamageEvent.DamageEvent]
            toDefender =
              if defenderIsPlaying
                then case target of
                  AttackTarget.OfPlayer defender ->
                    [damageEvent gs DamageKind.Combat attacker (Recipient.ToPlayer defender) power]
                else []
        if not (Combat.isBlocked attacker gs)
          then -- CR 510.1b: never blocked, so it hits what it is attacking.
            pure toDefender
          else case filter (\oid -> onBattlefield oid gs) (Set.toList recorded) of
            -- Blocked, but nothing is blocking it now. CR 702.19d: a trampler
            -- assigns everything to the defending player, "as though all blocking
            -- creatures have been assigned lethal damage". CR 510.1c: anything
            -- else assigns no combat damage at all -- not damage addressed to an
            -- object that is not there, which would still be recorded in the
            -- CR 608.2i history even though marking it is a no-op.
            [] -> pure (if trample then toDefender else [])
            -- CR 510.1c / 702.19b: a single blocker with no trample -- or trample but
            -- no power past its threshold -- is forced: all onto the blocker. A single
            -- trample blocker WITH excess fails this guard and falls to the prompt arm.
            [blocker]
              | not trample || power <= blockerThreshold gs attacker blocker ->
                  pure [damageEvent gs DamageKind.Combat attacker (Recipient.ToCreature blocker) power]
            blockers -> case Projection.controllerOf attacker gs of
              Nothing -> pure []
              -- CR 702.19b: the excess is assigned "as its controller chooses," so the
              -- chooser is the attacker's controller. Banding (CR 702.22j) inverts
              -- that -- the DEFENDING player chooses -- and is not implemented (#32).
              -- See the M2c spec, sections 4 and 8.
              Just pid -> do
                let decider = Decide.deciderFor pid gs
                    thresholdOf b = if trample then blockerThreshold gs attacker b else 0
                    blockerEntries = fmap (\b -> (Recipient.ToCreature b, thresholdOf b)) blockers
                    defenderEntry = case target of
                      AttackTarget.OfPlayer defender ->
                        if trample && defenderIsPlaying then [(Recipient.ToPlayer defender, 0 :: Natural)] else []
                    thresholds = Map.fromList (blockerEntries <> defenderEntry)
                chosen <-
                  Trans.lift
                    (Program.prompt (Prompt.AssignCombatDamage decider pid attacker thresholds power))
                -- CR 510.1e / 702.19b: reject-not-repair (NOT the CR 733 human-error
                -- rewind). An illegal answer assigns nothing. See the M2c spec, §4.
                let toEvent (recipient, n) = damageEvent gs DamageKind.Combat attacker recipient n
                    positive (_, n) = n > 0
                pure
                  ( if legalAssignment thresholds power chosen
                      then fmap toEvent (filter positive (Map.toList chosen))
                      else []
                  )

-- CR 510.1d: "A blocking creature assigns combat damage to the creatures it's
-- blocking. If it isn't currently blocking any creatures (if, for example, they
-- were destroyed or removed from combat), it assigns no combat damage."
--
-- That second sentence is the liveness filter on the ATTACKER, and it is the
-- mirror of the CR 510.1c filter attackerAssignment applies to the blockers:
-- CR 506.4 removes a permanent from combat when it leaves the battlefield, so
-- once the attacker is gone these creatures are blocking nothing. Reachable two
-- ways -- the attacker destroyed inside CR 510.4's two-step window, and its
-- owner leaving the game (CR 800.4a's first clause deletes the object).
--
-- The filter belongs HERE and not in Departure.objectsLeaveWith, for the same
-- reason CR 510.1c's does: Combat.blockers is keyed by the attacker and its key
-- IS the record of blocked-ness that CR 509.1h's last sentence protects, so
-- pruning it would be reading the rule backwards, and it would fix only the
-- departure route and not the destroyed one. Without the filter a blocker emits
-- a DamageEvent addressed to an object that is not on the battlefield: marking
-- it is a no-op once the id is gone, but the event still enters the CR 608.2i
-- history and still runs its own CR 616.1 replacement loop, where it could
-- spend a one-shot shield on damage the rules say was never assigned.
blockerAssignment :: GameState -> (ObjectId, Set.Set ObjectId) -> [DamageEvent.DamageEvent]
blockerAssignment gs (attacker, blockers) =
  let assign blocker = case Projection.powerOf blocker gs of
        Just p ->
          if p <= 0
            then []
            else [damageEvent gs DamageKind.Combat blocker (Recipient.ToCreature attacker) (Integer.toNaturalSaturating p)]
        Nothing -> []
   in if onBattlefield attacker gs
        then concatMap assign (Set.toList blockers)
        else []

-- CR 510.2: gather all combat damage before applying any of it (simultaneity).
gatherCombatDamage :: (ObjectId -> Bool) -> Game [DamageEvent.DamageEvent]
gatherCombatDamage assigns = do
  gs <- State.get
  let combat = GameState.combat gs
      attackers = filter (assigns . fst) (Map.toList (Combat.Type.attackers combat))
      blockers = Map.toList (fmap (Set.filter assigns) (Combat.Type.blockers combat))
  parts <- Monad.mapM (attackerAssignment gs) attackers
  let fromBlockers = concatMap (blockerAssignment gs) blockers
  pure (concat parts <> fromBlockers)

-- CR 120.1a: "Damage can't be dealt to an object that's not a battle, a
-- creature, or a planeswalker." Which of those a Recipient names is a question
-- only a slot-bound one raises: Recipient.ToObject is a permanent named
-- GENERICALLY (Pawl.Engine.Binding.became's entrant, Aether Flash's "it"), so what it
-- names has to be classified before an effect can deal damage to it, and it may
-- name nothing at all by the time the ability resolves (CR 608.2h). Nothing is
-- CR 120.1a's "can't": the effect deals no damage and no damage event is
-- proposed, so nothing downstream -- CR 616's replacement loop, CR 704.5h's
-- deathtouch scan, CR 608.2i's turn log -- sees an event that never happened.
--
-- ToCreature and ToPlayer pass through untouched. Both are produced by combat
-- (CR 510.1b-d, which name the blocking creature or the attacked player
-- outright) and by a CR 601.2c target chosen out of a typed Pool, so what they
-- name was already classified when the recipient was built; re-asking here
-- would be a second, later reading of the same question, which is what CR
-- 608.2b's target re-validation is for and this is not.
--
-- Battles and planeswalkers are missing from the classification. A battle has no
-- card type yet (#302); a planeswalker does (Jace Beleren), and CR 120.3c --
-- damage to it removes that many loyalty counters -- is unimplemented (#494),
-- which is why a ToObject naming one is dropped here rather than reclassified.
damageRecipient :: GameState -> Recipient.Recipient -> Maybe Recipient.Recipient
damageRecipient gs recipient = case recipient of
  Recipient.ToPlayer _ -> Just recipient
  Recipient.ToCreature _ -> Just recipient
  Recipient.ToObject oid ->
    if Projection.isCreatureOf oid gs
      then Just (Recipient.ToCreature oid)
      else Nothing

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
-- choosing which it covers. No such shield exists in the pool today (Fog is
-- unlimited-for-a-duration, not N-damage), so it stays card-driven (#58) -- but
-- the trip-wire is concrete, not hypothetical: `Monad.mapM Replacement.resolveDamage
-- events` below runs each event's CR 616.1 loop SEQUENTIALLY, and
-- Replacement.consume mutates GameState.replacements between siblings. The day a
-- card adds a `Uses.Once` `DamageR` shield meant to cover several sources at
-- once, it will be silently spent on whichever event in the batch happens to
-- sort first, rather than on the one the shielded player or controller would
-- have chosen -- the engine making CR 615.7's choice on a player's behalf, the
-- second invariant's violation. Watch for exactly that combination.
applyDamage :: [DamageEvent.DamageEvent] -> Game ()
applyDamage events = do
  survivors <- fmap Maybe.catMaybes (Monad.mapM Replacement.resolveDamage events)
  let markOne g ev = case DamageEvent.target ev of
        Recipient.ToCreature oid ->
          if DamageEvent.dealtByInfect ev
            then -- CR 120.3d / 702.90c: -1/-1 counters, no marked damage. Added
            -- directly (not via Event.putCounters): this is a consequence of
            -- a damage event that already ran the CR 616 replacement loop, so
            -- a "would put -1/-1 from infect" CR 614 sub-replacement is out of
            -- scope (#122).
              let addMinus obj = obj {Object.counters = Map.insertWith (+) CounterKind.MinusOneMinusOne (DamageEvent.amount ev) (Object.counters obj)}
               in g {GameState.objects = Map.adjust addMinus oid (GameState.objects g)}
            else
              let hurt obj = obj {Object.damage = Object.damage obj + DamageEvent.amount ev}
               in g {GameState.objects = Map.adjust hurt oid (GameState.objects g)}
        Recipient.ToPlayer pid ->
          -- The two poison diversions are different shapes and BOTH apply. CR
          -- 120.3b / 702.90b: infect REPLACES the damage's result with poison
          -- counters, so no life is lost. CR 120.3g / 702.164c: toxic gives the
          -- damaged player the source's total toxic value in poison "in
          -- addition to the damage's other results" -- on top of the life loss,
          -- or on top of infect's poison when a source has both.
          --
          -- The damaged PLAYER gets the counters, not the source's controller,
          -- who is merely who performs it (CR 120.3b/120.3g's phrasing). And
          -- toxic is scoped to COMBAT damage, so a noncombat event's captured
          -- value is deliberately ignored.
          let toxic = case DamageEvent.kind ev of
                DamageKind.Combat -> DamageEvent.dealtByToxic ev
                DamageKind.Noncombat -> 0
              givePoison n player =
                if n == 0
                  then player
                  else player {Player.counters = Map.insertWith (+) PlayerCounterKind.Poison n (Player.counters player)}
              drain player = player {Player.life = Player.life player - toInteger (DamageEvent.amount ev)}
              hit player =
                if DamageEvent.dealtByInfect ev
                  then givePoison (DamageEvent.amount ev + toxic) player
                  else givePoison toxic (drain player)
           in g {GameState.players = Map.adjust hit pid (GameState.players g)}
        -- CR 120.1a, and defensive: combat never builds this shape (CR 510.1b-d
        -- name a creature or a player), and the one producer that can --
        -- Resolve's DealDamage arm, reading a slot that names a permanent
        -- generically -- runs it through damageRecipient above first, which
        -- turns it into ToCreature or into no event at all. Marking damage here
        -- would be the wrong answer if anything did reach it: nothing has said
        -- the object is a creature.
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
      alive oid = onBattlefield oid gs
  case Combat.Type.struckFirst combat of
    Nothing
      -- CR 510.4 does not apply: no striker, so one step and everyone deals.
      | Set.null strikers -> do
          dealWave alive
          pure False
      -- CR 510.4: a striker is present. This is the first of two steps; only
      -- first strikers and double strikers deal, and a second step follows.
      | otherwise -> do
          State.modify' (\g -> g {GameState.combat = (GameState.combat g) {Combat.Type.struckFirst = Just strikers}})
          dealWave (\oid -> alive oid && Set.member oid strikers)
          pure True
    -- CR 510.4 second step: those that had neither first strike nor double strike
    -- as the first step began (not in the snapshot), plus those that currently
    -- have double strike -- and are still on the battlefield.
    Just snapshot -> do
      dealWave (\oid -> alive oid && (Set.notMember oid snapshot || Projection.hasKeyword Keyword.DoubleStrike oid gs))
      pure False

-- Gather this wave's damage under `assigns` and apply it.
dealWave :: (ObjectId -> Bool) -> Game ()
dealWave assigns = do
  assignment <- gatherCombatDamage assigns
  applyDamage assignment

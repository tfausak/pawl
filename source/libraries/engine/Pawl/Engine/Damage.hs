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
import qualified Pawl.Extra.Natural as Natural
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
-- CR 514.2 covers phased-out permanents too. Being selective is how a stale mark
-- survives.
removeAllDamage :: GameState -> GameState
removeAllDamage gs =
  let clear obj = obj {Object.damage = 0}
   in gs {GameState.objects = fmap clear (GameState.objects gs)}

-- CR 506.4: a permanent is removed from combat if it leaves the battlefield. The
-- liveness test every combat-damage read shares, because the combat record
-- outlives the objects it names. Two ways off the battlefield -- destroyed inside
-- CR 510.4's two-step window, or deleted outright when its owner left the game
-- (CR 800.4a's first clause) -- so the predicate is on the ZONE, not on mere
-- existence: Event.destroy leaves the object in the graveyard.
onBattlefield :: ObjectId -> GameState -> Bool
onBattlefield oid gs = case Game.lookupObject oid gs of
  Just obj -> Object.zone obj == Zone.Battlefield
  Nothing -> False

-- CR 510.1e / 702.19b, as a pure predicate over the whole assignment. Legal iff
-- it totals power, uses only legal recipients, and -- the trample implication --
-- the defender got damage ONLY if every blocker is at its lethal threshold. The
-- threshold is NOT a per-blocker floor: a blocker may be under-assigned as long
-- as the defender then gets nothing.
legalAssignment :: Map.Map Recipient.Recipient Natural -> Natural -> Map.Map Recipient.Recipient Natural -> Bool
legalAssignment thresholds power answer =
  let assigned r = Map.findWithDefault 0 r answer
      totalsPower = sum (Map.elems answer) == power
      onlyLegal = all (\r -> Map.member r thresholds) (Map.keys answer)
      isDefender r = case r of
        Recipient.ToPlayer _ -> True
        Recipient.ToCreature _ -> False
        -- CR 702.19b: the excess is assigned among the blocking creatures and
        -- what the creature is attacking, so an attacked planeswalker is on THIS
        -- side of the split, not the blockers'. CR 702.19f is why it is the only
        -- non-blocker recipient the map can hold for such an attacker --
        -- attackerAssignment never offers the player alongside it, and the gate
        -- below reads the whole non-blocker share either way.
        Recipient.ToPlaneswalker _ -> True
        Recipient.ToObject _ -> False
      defenderAmount = sum (Map.elems (Map.filterWithKey (\r _ -> isDefender r) answer))
      blockerThresholds = Map.filterWithKey (\r _ -> not (isDefender r)) thresholds
      everyBlockerLethal = all (\(r, t) -> assigned r >= t) (Map.toList blockerThresholds)
      defenderGated = defenderAmount == 0 || everyBlockerLethal
   in totalsPower && onlyLegal && defenderGated

-- CR 702.19b / 702.2c: a blocker's lethal threshold is toughness minus marked
-- damage -- but 702.2c makes any nonzero assignment by a deathtouch source
-- lethal, so a deathtouch attacker needs only 1 (0 if the blocker is already
-- lethal). Read through the projection, the same way the CR 704.5h SBA reads it.
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
-- SBA or a later reader asks. CR 702.2e, CR 702.90d and CR 702.15c say so
-- outright for deathtouch, infect and lifelink. Rule 702.164 has NO such clause,
-- so toxic's total value (CR 702.164b) is captured by analogy rather than by
-- citation -- observably the same today, since applyDamage runs in the same
-- instant, and it keeps the CR 608.2i record self-contained.
--
-- Lifelink's rider carries WHO rather than WHETHER, because CR 702.15b's answer
-- is a player. Projection.controllerWithLastKnown delegates to controllerOf,
-- which is both of that rule's clauses at once and answers in any zone, which is
-- what CR 702.15d needs.
--
-- Read through the ...WithLastKnown pair rather than the plain readers, because
-- the source may already have CEASED by the time it deals damage -- Ghitu
-- Fire-Eater sacrifices itself to pay for the ability that then deals its
-- damage. The plain readers answer False, 0 and Nothing for an id that names
-- nothing, which would silently report every rider absent rather than as it last
-- was, against CR 702.2e, CR 702.15c, CR 702.90d and CR 608.2h.
--
-- One fallback for all four riders, not four: they are read here at a single
-- site off two readers that share one liveness test (Projection.lastKnownOf), so
-- deathtouch and lifelink cannot come to disagree about whether the source is
-- still there. Every damage the engine deals is built here, so no assignment
-- site can capture two riders and forget the third.
damageEvent :: GameState -> DamageKind.DamageKind -> ObjectId -> Recipient.Recipient -> Natural -> DamageEvent.DamageEvent
damageEvent gs kind source target amount =
  let keywords = Projection.keywordsWithLastKnown source gs
      has keyword = Map.member keyword keywords
      -- CR 702.164b: toxic is parameterized, so its rider is the SUM of every
      -- instance's N rather than a membership test -- Projection.totalToxic's
      -- fold, taken over the same map so it shares the fallback.
      toxic = Projection.toxicIn keywords
   in DamageEvent.MkDamageEvent
        { DamageEvent.source = source,
          DamageEvent.target = target,
          DamageEvent.amount = amount,
          DamageEvent.dealtByDeathtouch = has Keyword.Deathtouch,
          DamageEvent.dealtByInfect = has Keyword.Infect,
          DamageEvent.dealtByToxic = toxic,
          DamageEvent.dealtByLifelink =
            if has Keyword.Lifelink
              then Projection.controllerWithLastKnown source gs
              else Nothing,
          DamageEvent.kind = kind
        }

-- CR 510.1b: what an attacking creature assigns its combat damage TO, or Nothing
-- when it is not currently attacking anything and so assigns none.
--
-- The two arms answer to different rules, which is why this is a case and not one
-- liveness test:
--
--   * a player leaving is CR 800.4e -- the creature is still attacking them, and
--     the damage is what goes missing.
--   * a planeswalker's is CR 506.4: it is REMOVED FROM COMBAT and stops being
--     attacked, so by CR 506.4c the creature keeps attacking nothing at all, and
--     CR 510.1b then gives it nothing to assign to. Combat.stillAttacked is that
--     rule.
--
-- Read at ASSIGNMENT and at every place assignment can name a recipient (the
-- unblocked/trample-through event and the CR 702.19b threshold map the prompt
-- offers), never as a filter over the finished batch: filtering afterwards would
-- let the attacker's controller legally assign trample excess to something that
-- cannot take it and then lose that damage, rather than never offering the
-- choice.
combatRecipient :: GameState -> AttackTarget -> Maybe Recipient.Recipient
combatRecipient gs target = case target of
  AttackTarget.OfPlayer defender ->
    if List.elem defender (Game.stillPlaying gs)
      then Just (Recipient.ToPlayer defender)
      else Nothing
  AttackTarget.OfPlaneswalker oid ->
    if Combat.stillAttacked oid gs
      then Just (Recipient.ToPlaneswalker oid)
      else Nothing

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
            -- CR 510.1b: what this creature is attacking, if it is still
            -- attacking anything. Reachable both ways in the pool -- a defending
            -- player conceding mid-combat (CR 800.4e), and an attacked
            -- planeswalker burned off the battlefield (CR 506.4).
            attacked = combatRecipient gs target
        -- Whether it is BLOCKED and WHO is blocking it are two questions, and the
        -- branch below asks each of the reader that answers it. CR 509.1h makes
        -- blocked-ness a status the declaration confers (Combat.isBlocked, the
        -- attacker's key in the map), which survives every blocker leaving; CR
        -- 510.1c then gives damage only to the creatures CURRENTLY blocking. The
        -- two answers come apart both ways a blocker can go: destroyed leaves it
        -- in `recorded` (the liveness filter below is the only site that screens
        -- it out), regenerated takes it out of `recorded` while the key stays.
        -- Reading emptiness as unblocked gets the second case wrong and lets the
        -- attacker hit the defending player.
        let recorded = Combat.blockersOf attacker gs
            toDefender :: [DamageEvent.DamageEvent]
            toDefender =
              fmap (\recipient -> damageEvent gs DamageKind.Combat attacker recipient power) (Maybe.maybeToList attacked)
        if not (Combat.isBlocked attacker gs)
          then -- CR 510.1b: never blocked, so it hits what it is attacking.
            pure toDefender
          else case filter (\oid -> onBattlefield oid gs) (Set.toList recorded) of
            -- Blocked, but nothing is blocking it now. CR 702.19d: a trampler
            -- assigns everything to the defending player. CR 510.1c: anything
            -- else assigns no combat damage at all -- not damage addressed to an
            -- object that is not there, which would still be recorded in the CR
            -- 608.2i history even though marking it is a no-op.
            [] -> pure (if trample then toDefender else [])
            -- CR 510.1c / 702.19b: a single blocker with no trample -- or trample
            -- but no power past its threshold -- is forced: all onto the blocker.
            -- A single trample blocker WITH excess falls to the prompt arm.
            [blocker]
              | not trample || power <= blockerThreshold gs attacker blocker ->
                  pure [damageEvent gs DamageKind.Combat attacker (Recipient.ToCreature blocker) power]
            blockers -> case Projection.controllerOf attacker gs of
              Nothing -> pure []
              -- CR 702.19b: the attacker's controller chooses how to assign the
              -- excess. Banding (CR 702.22j) inverts that -- the DEFENDING player
              -- chooses -- and is not implemented (#32).
              Just pid -> do
                let decider = Decide.deciderFor pid gs
                    thresholdOf b = if trample then blockerThreshold gs attacker b else 0
                    blockerEntries = fmap (\b -> (Recipient.ToCreature b, thresholdOf b)) blockers
                    -- CR 702.19b: the trample-through recipient is whatever the
                    -- creature is attacking, at threshold 0 -- there is no
                    -- minimum to assign to it. CR 702.19c's larger threshold (a
                    -- planeswalker's loyalty) belongs to trample OVER
                    -- PLANESWALKERS, which is not a keyword pawl has (#539), and
                    -- CR 702.19f is what keeps plain trample from ever putting
                    -- the defending PLAYER here alongside a planeswalker.
                    defenderEntry =
                      if trample
                        then fmap (\recipient -> (recipient, 0 :: Natural)) (Maybe.maybeToList attacked)
                        else []
                    thresholds = Map.fromList (blockerEntries <> defenderEntry)
                chosen <-
                  Trans.lift
                    (Program.prompt (Prompt.AssignCombatDamage decider pid attacker thresholds power))
                -- CR 510.1e / 702.19b: reject-not-repair (NOT the CR 733
                -- human-error rewind). An illegal answer assigns nothing.
                let toEvent (recipient, n) = damageEvent gs DamageKind.Combat attacker recipient n
                    positive (_, n) = n > 0
                pure
                  ( if legalAssignment thresholds power chosen
                      then fmap toEvent (filter positive (Map.toList chosen))
                      else []
                  )

-- CR 510.1d: a blocking creature assigns combat damage to the creatures it's
-- blocking, and none at all if it isn't currently blocking any.
--
-- That second sentence is the liveness filter on the ATTACKER, and it is the
-- mirror of the CR 510.1c filter attackerAssignment applies to the blockers: CR
-- 506.4 removes a permanent from combat when it leaves the battlefield.
-- Reachable two ways -- the attacker destroyed inside CR 510.4's two-step window,
-- and its owner leaving the game (CR 800.4a's first clause deletes the object).
--
-- The filter belongs HERE and not in Departure.objectsLeaveWith, for the same
-- reason CR 510.1c's does: Combat.blockers is keyed by the attacker and its key
-- IS the record of blocked-ness that CR 509.1h protects, so pruning it would be
-- reading the rule backwards, and it would fix only the departure route. Without
-- the filter a blocker emits a DamageEvent addressed to an object that is not on
-- the battlefield: marking it is a no-op, but the event still enters the CR
-- 608.2i history and still runs its own CR 616.1 replacement loop, where it
-- could spend a one-shot shield on damage the rules say was never assigned.
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

-- CR 120.1a: damage can't be dealt to an object that's not a battle, a creature,
-- or a planeswalker. Which of those a Recipient names is a question only a
-- slot-bound one raises: Recipient.ToObject is a permanent named GENERICALLY
-- (Pawl.Engine.Binding.became's entrant, Aether Flash's "it"), so what it names
-- has to be classified before an effect can deal damage to it, and it may name
-- nothing at all by the time the ability resolves (CR 608.2h). Nothing is CR
-- 120.1a's "can't": no damage event is proposed, so nothing downstream -- CR
-- 616's replacement loop, CR 704.5h's deathtouch scan, CR 608.2i's turn log --
-- sees an event that never happened.
--
-- ToCreature, ToPlaneswalker and ToPlayer pass through untouched. Each is
-- produced either by combat (CR 510.1b-d) or by a CR 601.2c target chosen out of
-- a typed Pool, so what it names was already classified when the recipient was
-- built; re-asking here would be a second, later reading of the same question,
-- which is what CR 608.2b's target re-validation is for and this is not.
--
-- Only battles are missing from the classification, and only because no card type
-- for one exists yet (#302); CR 120.3h is what it would need.
--
-- The creature test comes first, and for a permanent that is both a creature and
-- a planeswalker that is the wrong answer -- CR 120.3c and CR 120.3e both apply
-- and one Recipient cannot carry both (#503). Unreachable today: nothing in the
-- pool prints both card types, and no effect in it adds a creature type to a
-- planeswalker.
damageRecipient :: GameState -> Recipient.Recipient -> Maybe Recipient.Recipient
damageRecipient gs recipient = case recipient of
  Recipient.ToPlayer _ -> Just recipient
  Recipient.ToCreature _ -> Just recipient
  Recipient.ToPlaneswalker _ -> Just recipient
  Recipient.ToObject oid
    | Projection.isCreatureOf oid gs -> Just (Recipient.ToCreature oid)
    | Projection.isPlaneswalkerOf oid gs -> Just (Recipient.ToPlaneswalker oid)
    | otherwise -> Nothing

-- CR 120.3e / 120.3a: mark damage on creatures, drain life from players -- AND
-- record each event into GameState.events. The change-and-emit funnel for
-- combat's two waves and resolving effects alike.
--
-- CR 615 / 616: EACH event in the batch runs its OWN CR 616.1 loop, and the
-- survivors are applied together. Simultaneity is preserved as a SCHEDULING
-- property; the loop's unit stays one event, uniform with the other five
-- classes, which is what CR 614.5 and CR 615.10 both describe.
--
-- The whole batch goes through Replacement.resolveDamageBatch rather than
-- through resolveDamage one event at a time, and CR 615.7 is the reason: a
-- prevent-the-next-N shield (Mending Hands) is ONE resource allocated across
-- several simultaneous events, and the shielded side chooses which damage it
-- prevents. Those loops still run sequentially and the shield is still spent by
-- whichever runs first -- what changed is that the shielded side, not the batch's
-- gather order, says which that is.
applyDamage :: [DamageEvent.DamageEvent] -> Game ()
applyDamage events = do
  survivors <- Replacement.resolveDamageBatch events
  let markOne g ev = case DamageEvent.target ev of
        Recipient.ToCreature oid ->
          if DamageEvent.dealtByInfect ev
            then -- CR 120.3d / 702.90c: -1/-1 counters, no marked damage. Added
            -- directly (not via Replacement.putCounters): this is a consequence of
            -- a damage event that already ran the CR 616 replacement loop, so
            -- a "would put -1/-1 from infect" CR 614 sub-replacement is out of
            -- scope (#122).
              let addMinus obj = obj {Object.counters = Map.insertWith (+) CounterKind.MinusOneMinusOne (DamageEvent.amount ev) (Object.counters obj)}
               in g {GameState.objects = Map.adjust addMinus oid (GameState.objects g)}
            else
              let hurt obj = obj {Object.damage = Object.damage obj + DamageEvent.amount ev}
               in g {GameState.objects = Map.adjust hurt oid (GameState.objects g)}
        -- CR 306.8 / CR 120.3c: damage dealt to a planeswalker removes that many
        -- loyalty counters. Removed DIRECTLY, for the reason the infect arm above
        -- gives: this is a result of a damage event that has already run its CR
        -- 616.1 loop, so a "would remove counters" sub-replacement is out of
        -- scope (#122) -- and CR 614.16 scales counters an effect PUTS on, never
        -- removal.
        --
        -- Floored at 0 rather than wrapped, because Object.counters is Natural:
        -- CR 306.5c makes loyalty the COUNT of loyalty counters. CR 704.5i then
        -- reads the 0 and buries it; nothing here destroys anything (CR 120.5).
        Recipient.ToPlaneswalker oid ->
          let have obj = Map.findWithDefault 0 CounterKind.Loyalty (Object.counters obj)
              strip obj =
                obj
                  { Object.counters =
                      Map.insert
                        CounterKind.Loyalty
                        (Natural.minusSaturating (have obj) (DamageEvent.amount ev))
                        (Object.counters obj)
                  }
           in g {GameState.objects = Map.adjust strip oid (GameState.objects g)}
        Recipient.ToPlayer pid ->
          -- The two poison diversions are different shapes and BOTH apply. CR
          -- 120.3b / 702.90b: infect REPLACES the damage's result with poison
          -- counters, so no life is lost. CR 120.3g / 702.164c: toxic gives the
          -- damaged player the source's total toxic value in poison in addition
          -- to the damage's other results -- on top of the life loss, or on top
          -- of infect's poison when a source has both.
          --
          -- The damaged PLAYER gets the counters, not the source's controller,
          -- who merely performs it (CR 120.3b/120.3g). And toxic is scoped to
          -- COMBAT damage, so a noncombat event's captured value is ignored.
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
        -- name a creature, a player or a planeswalker), and the one producer that
        -- can -- Resolve's DealDamage arm, naming a permanent generically -- runs
        -- every recipient through damageRecipient above first. Doing anything
        -- here would be the wrong answer if anything did reach it: nothing has
        -- said which of CR 120.3's results applies.
        Recipient.ToObject _ -> g
      -- CR 120.3f: lifelink damage gains its source's controller that much life,
      -- IN ADDITION to the damage's other results. A second pass over the same
      -- survivors, deliberately not a branch inside markOne: "in addition" is
      -- then structural, and no arm of CR 120.3 above can be turned into
      -- "instead" by an edit here.
      --
      -- Every survivor, whatever it was dealt to. CR 702.15b hangs the gain off
      -- the SOURCE, so which of CR 120.3's results the damage had is none of this
      -- pass's business.
      --
      -- One adjustment per event rather than one per player: CR 702.15e makes
      -- simultaneous lifelink sources cause SEPARATE life gain events (CR
      -- 119.9-10), and summing them first would be one event.
      --
      -- Over `survivors`, so CR 615.6's prevented event gains nobody anything --
      -- the same reading toxic's arm above takes.
      gainOne g ev = case DamageEvent.dealtByLifelink ev of
        Nothing -> g
        Just pid ->
          let gain player = player {Player.life = Player.life player + toInteger (DamageEvent.amount ev)}
           in g {GameState.players = Map.adjust gain pid (GameState.players g)}
  -- CR 608.2i: each surviving event is RECORDED, not enqueued. Sba consumes by
  -- bumping GameState.damageScannedThrough; the record survives the check.
  State.modify'
    ( \gs ->
        let marked = List.foldl' markOne gs survivors
            gained = List.foldl' gainOne marked survivors
         in List.foldl' (\g ev -> Event.recordEvent (GameEvent.DamageDealt ev) g) gained survivors
    )

-- Deal one combat damage step, returning True iff this was the FIRST of two --
-- i.e. a second combat damage step must be spliced (CR 510.4).
--
-- Which creatures assign is read LIVE off the projection at this boundary, never
-- precomputed. `struckFirst` both routes the wave and records CR 510.4's "had
-- first strike or double strike as the first step began" snapshot. Only
-- creatures still on the battlefield assign, so a striker killed in the first
-- step is gone for the second.
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

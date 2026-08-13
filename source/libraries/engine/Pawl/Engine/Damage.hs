module Pawl.Engine.Damage where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Defender as Defender
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.AttackTarget (AttackTarget)
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prevention as Prevention
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

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
-- (CR 800.4a's first clause) -- so the predicate is not mere existence:
-- Event.destroy leaves the object in the graveyard.
--
-- Battlefield MEMBERSHIP and not Object.zone, which differ for exactly one thing:
-- CR 702.26b's phased-out permanents, which the game treats as not existing while
-- their zone still reads Zone.Battlefield (CR 702.26d). Rule 702.26b's own last
-- sentence puts such a permanent in the same position as one that left, so the
-- set is the reading CR 506.4 wants.
onBattlefield :: ObjectId -> GameState -> Bool
onBattlefield oid gs = Set.member oid (GameState.battlefield gs)

-- CR 510.1e, one of the rule's two halves: an answer is well formed iff it totals
-- the creature's power and names only recipients it was offered. Both questions
-- are about THIS creature's answer alone, which is what separates them from the
-- trample gates below -- rule 510.1e's own parenthesis, "not solely the damage
-- assignment of any individual attacking or blocking creature", is about those.
wellFormedAssignment :: Map.Map Recipient.Recipient Natural -> Natural -> Map.Map Recipient.Recipient Natural -> Bool
wellFormedAssignment thresholds power answer =
  sum (Map.elems answer) == power
    && all (\r -> Map.member r thresholds) (Map.keys answer)

-- Which gate a recipient sits behind, read off which recipient it IS, because CR
-- 702.19b's own sentence is that case split: the blocking creatures, then "the
-- player, planeswalker, or battle the creature is attacking", then -- only under
-- CR 702.19c -- that planeswalker's controller.
--
-- Recipient.ToObject shares the blockers' tier, i.e. is ungated: combat never
-- builds one (CR 510.1b-d name a creature, a player, a planeswalker or a battle),
-- so no assignment reaches here holding one.
tier :: Recipient.Recipient -> Int
tier r = case r of
  Recipient.ToCreature _ -> 0
  Recipient.ToObject _ -> 0
  Recipient.ToPlaneswalker _ -> 1
  Recipient.ToBattle _ -> 1
  Recipient.ToPlayer _ -> 2

-- CR 510.1e's other half: the gates the trample rules stack, as ORDERED TIERS
-- rather than one split:
--
--   * CR 702.19b: nothing may go past the blocking creatures until EVERY one of
--     them is at its lethal threshold. Not a per-blocker floor -- a blocker may
--     be under-assigned as long as nothing spills past it.
--   * CR 702.19c: the attacked planeswalker's CONTROLLER sits behind a second
--     gate, that planeswalker's own threshold (its loyalty).
--
-- Vacuous for every attacker but a trampler: attackerAssignment gives the
-- attacked permanent threshold 0 without trample over planeswalkers, and CR
-- 702.19f is why it never offers the defending player alongside a planeswalker at
-- all, so the middle tier is either empty or free and the answer turns on the
-- blockers alone.
--
-- WHETHER a gate is cleared is asked of `assigned` -- what every attacking
-- creature is assigning in this combat damage step -- and not of this creature's
-- own answer, which is the last sentence of both CR 702.19b and CR 702.19c: "take
-- into account ... damage from other creatures that's being assigned during the
-- same combat damage step". The THRESHOLD stays this creature's own reading of
-- the bar (blockerThreshold's toughness-minus-marked, the planeswalker's
-- loyalty), because that half of the same sentence -- "damage already marked on
-- the creature" -- is knowable when the division is offered, where the other
-- half is not settled until the whole step's assignment is announced.
--
-- `lethal` is CR 702.2c's other reading of the same totals: a creature that some
-- nonzero deathtouch assignment named is AT its bar whatever the number, since
-- that assignment "is considered to be lethal damage for the purposes of
-- determining if excess damage is being dealt". The rule is about the recipient
-- and not about who is spilling past it, so it clears the gate for every creature
-- assigning this step -- deathtouchedRecipients reads it off the step's events.
tiersCleared :: Map.Map Recipient.Recipient Natural -> Map.Map Recipient.Recipient Natural -> Set.Set Recipient.Recipient -> Map.Map Recipient.Recipient Natural -> Bool
tiersCleared thresholds assigned lethal answer =
  let -- Every recipient in a STRICTLY EARLIER tier is at its threshold. Vacuously
      -- true at tier 0, which is CR 702.19b's "need not assign lethal damage to
      -- all those blocking creatures".
      earlierTiersMet t =
        all
          (\(r, threshold) -> Set.member r lethal || Map.findWithDefault 0 r assigned >= threshold)
          (filter (\(r, _) -> tier r < t) (Map.toList thresholds))
   in all (\(r, n) -> n == 0 || earlierTiersMet (tier r)) (Map.toList answer)

-- CR 702.2c: which recipients an assignment has given lethal damage by being
-- made at all -- "any nonzero amount of combat damage assigned to a creature by
-- a source with deathtouch".
--
-- A CREATURE only. The rule says so, and the bar CR 702.19c states over a
-- planeswalker is a count of loyalty counters (CR 306.5c) that deathtouch says
-- nothing about. A player and a battle carry threshold 0 and so gate nothing
-- either way, and combat builds no Recipient.ToObject at all (CR 510.1b-d).
--
-- Read off the announced events rather than re-derived from the assigning
-- creature, so it uses the same DamageEvent.dealtByDeathtouch rider the CR 704.5h
-- SBA will (damageEvent captures it as the damage is dealt, per CR 702.2e).
deathtouchedRecipients :: [DamageEvent.DamageEvent] -> Set.Set Recipient.Recipient
deathtouchedRecipients events =
  Set.fromList
    [ target
    | event <- events,
      DamageEvent.dealtByDeathtouch event,
      -- Every combat event is built with amount > 0 (CR 510.1a), so this is a
      -- fence on "nonzero" rather than a reachable filter.
      DamageEvent.amount event > 0,
      target <- case DamageEvent.target event of
        Recipient.ToCreature _ -> [DamageEvent.target event]
        Recipient.ToObject _ -> []
        Recipient.ToPlaneswalker _ -> []
        Recipient.ToBattle _ -> []
        Recipient.ToPlayer _ -> []
    ]

-- CR 702.19b / 702.2c: a blocker's lethal threshold is toughness minus marked
-- damage -- but 702.2c makes any nonzero assignment by a deathtouch source
-- lethal, so a deathtouch attacker needs only 1 (0 if the blocker is already
-- lethal). Read through the projection, the same way the CR 704.5h SBA reads it.
--
-- THIS attacker's own reading of the bar, which is what the division is OFFERED
-- at. Another creature's deathtouch damage clears the same bar for everyone (CR
-- 702.2c, in tiersCleared's `lethal`), but only once the whole step has been
-- announced, where this much is knowable when the offer is made.
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
-- SBA or a later reader asks. CR 702.2e, CR 702.90d, CR 702.15c and CR 702.80b
-- say so outright for deathtouch, infect, lifelink and wither. Rule 702.164 has
-- NO such clause, so toxic's total value (CR 702.164b) is captured by analogy
-- rather than by citation -- observably the same today, since applyDamage runs
-- in the same instant, and it keeps the CR 608.2i record self-contained.
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
-- was, against CR 702.2e, CR 702.15c, CR 702.90d, CR 702.80b and CR 608.2h.
--
-- One fallback for all five riders, not five: they are read here at a single
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
          DamageEvent.dealtByWither = has Keyword.Wither,
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
-- CR 702.19e is the stated exception to that second arm, and it is why this asks
-- about the ATTACKER at all: a creature with trample over planeswalkers whose
-- planeswalker was removed from combat assigns to the defending player instead of
-- nothing. Rule 702.19e's own last sentence is why the answer is a recipient
-- rather than a rewritten attack target -- "it does not cause the creature to be
-- attacking that player" -- so Combat.attackers still names the planeswalker and
-- CR 506.4c's record is untouched.
--
-- The defending player is read off the combat record (Combat.defender) and not
-- re-derived from the planeswalker, which is gone -- CR 508.5's second sentence,
-- the same reading Defender.playerOf now takes for every caller: pawl's combat has
-- ONE defending player (see Pawl.Types.Combat) and an attack on a planeswalker is
-- declared against that player's planeswalkers (CR 508.1b), so the record's player
-- is the controller it had before it was removed from combat.
--
-- Read at ASSIGNMENT and at every place assignment can name a recipient (the
-- unblocked/trample-through event and the CR 702.19b threshold map the prompt
-- offers), never as a filter over the finished batch: filtering afterwards would
-- let the attacker's controller legally assign trample excess to something that
-- cannot take it and then lose that damage, rather than never offering the
-- choice.
combatRecipient :: GameState -> ObjectId -> AttackTarget -> Maybe Recipient.Recipient
combatRecipient gs attacker target =
  let -- CR 800.4e: a player who has left the game is nobody to assign to.
      stillPlaying pid =
        if List.elem pid (Game.stillPlaying gs)
          then Just (Recipient.ToPlayer pid)
          else Nothing
   in case target of
        AttackTarget.OfPlayer defender -> stillPlaying defender
        AttackTarget.OfPlaneswalker oid
          | Combat.stillAttacked oid gs -> Just (Recipient.ToPlaneswalker oid)
          | Projection.hasKeyword Keyword.TrampleOverPlaneswalkers attacker gs ->
              stillPlaying =<< Combat.Type.defender (GameState.combat gs)
          | otherwise -> Nothing
        -- CR 310.5 / 506.4, the planeswalker arm's twin: a battle that has left the
        -- battlefield is removed from combat and stops being attacked, so CR 510.1b
        -- gives the creature nothing to assign to. No CR 702.19e for a battle: that
        -- rule names a planeswalker only.
        AttackTarget.OfBattle oid ->
          if Combat.stillAttackedBattle oid gs
            then Just (Recipient.ToBattle oid)
            else Nothing

-- What one attacking creature ANNOUNCES it assigns, as damage events carrying the
-- source, paired with the thresholds its CR 702.19b/702.19c gates are stated over.
-- CR 510.1a: a creature that would assign 0 or less assigns none, so events all
-- carry amount > 0.
--
-- An announcement and not the finished assignment, because the gates are a
-- question about the whole step (tiersCleared) and this function sees one
-- creature: gatherCombatDamage asks them once every attacker has announced. The
-- thresholds are empty wherever the assignment was FORCED and no gate can bite --
-- an untrampled creature has nowhere to spill to.
--
-- `contested` is whether another creature assigning damage this step can reach one
-- of the recipients a threshold is stated over (contestedAssignment). It is what
-- keeps the two forced arms below sound now that the gates read the whole step: a
-- division this creature could not clear alone may be one the step clears between
-- them, so "the rules leave nothing to ask" holds only when no other creature can
-- pay any part of the same bar. Where one can, the question is asked -- and the
-- answer may turn out to have been forced after all, which is the direction CR
-- 510.1 itself takes by announcing the whole assignment at once.
attackerAssignment :: GameState -> Bool -> (ObjectId, AttackTarget) -> Game ([DamageEvent.DamageEvent], Map.Map Recipient.Recipient Natural)
attackerAssignment gs contested (attacker, target) = case Projection.powerOf attacker gs of
  Nothing -> pure ([], Map.empty)
  Just p ->
    if p <= 0
      then pure ([], Map.empty)
      else do
        let power :: Natural
            power = Integer.toNaturalSaturating p
            -- CR 702.19c makes trample over planeswalkers a VARIANT of trample --
            -- it assigns "as described in rule 702.19b, with one exception" -- and
            -- CR 702.19d names the two side by side, so the variant tramples
            -- whether or not the creature also has plain trample. Not separately
            -- observable in the pool: Thrasta, its only printing, has both.
            overPlaneswalkers = Projection.hasKeyword Keyword.TrampleOverPlaneswalkers attacker gs
            trample = overPlaneswalkers || Projection.hasKeyword Keyword.Trample attacker gs
            -- CR 510.1b: what this creature is attacking, if it is still
            -- attacking anything. Reachable both ways in the pool -- a defending
            -- player conceding mid-combat (CR 800.4e), and an attacked
            -- planeswalker burned off the battlefield (CR 506.4).
            attacked = combatRecipient gs attacker target
            -- CR 508.5 / CR 310.8d, shared with the landwalk reading in
            -- Defender.playerOf so the two cannot drift. Read once, for CR
            -- 702.19c's third recipient below and for CR 702.22j's chooser.
            defending = Defender.playerOf target gs
            -- CR 702.19b: the trample-through recipient is whatever the creature
            -- is attacking, at threshold 0 -- there is no minimum to assign to it.
            --
            -- CR 702.19c raises that threshold to the attacked planeswalker's
            -- LOYALTY (CR 306.5c) and puts that planeswalker's controller behind
            -- it at threshold 0. Both entries in one map is what makes
            -- tiersCleared's second tier reachable, and CR 702.19f is what
            -- keeps every other attacker from ever getting the player entry.
            --
            -- The threshold is this planeswalker's own loyalty, whatever other
            -- creatures are assigning to it: rule 702.19c's last sentence is about
            -- CHECKING the bar, which tiersCleared does against the whole step.
            defenderEntries :: [(Recipient.Recipient, Natural)]
            defenderEntries = case attacked of
              Just recipient@(Recipient.ToPlaneswalker oid)
                | overPlaneswalkers ->
                    (recipient, Cost.loyaltyCountersOn oid gs)
                      : fmap (\pid -> (Recipient.ToPlayer pid, 0)) (Maybe.maybeToList defending)
              _ -> fmap (\recipient -> (recipient, 0)) (Maybe.maybeToList attacked)
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
            allToAttacked :: [DamageEvent.DamageEvent]
            allToAttacked =
              fmap (\recipient -> damageEvent gs DamageKind.Combat attacker recipient power) (Maybe.maybeToList attacked)
            -- With no blocker left to gate, everything the creature assigns goes
            -- to what it is attacking -- except under CR 702.19c, where the
            -- attacked planeswalker's controller stands behind it and the split is
            -- a real choice. So this is an action and not a list.
            --
            -- Forced, and so not asked, in the two shapes where the rules leave
            -- nothing to ask: one recipient, and a planeswalker whose loyalty (CR
            -- 702.19c's threshold) is at least the creature's power, which absorbs
            -- everything it could assign -- and, for that second shape, only while
            -- nothing else this step can pay part of that loyalty.
            toDefender :: Game ([DamageEvent.DamageEvent], Map.Map Recipient.Recipient Natural)
            toDefender = case defenderEntries of
              _ : _ : _
                | contested || power > sum (fmap snd defenderEntries) ->
                    case Projection.controllerOf attacker gs of
                      Nothing -> pure ([], Map.empty)
                      -- No blocker, so CR 702.22j's inversion cannot apply and CR
                      -- 702.19c's "as the attacking creature's controller chooses"
                      -- stands unqualified.
                      Just pid -> divide pid defenderEntries
              _ -> pure (allToAttacked, Map.empty)
            divide = divideAssignment gs attacker power
        if not (Combat.isBlocked attacker gs)
          then -- CR 510.1b: never blocked, so it hits what it is attacking.
            toDefender
          else case filter (\oid -> onBattlefield oid gs) (Set.toList recorded) of
            -- Blocked, but nothing is blocking it now. CR 702.19d: a trampler
            -- assigns everything to the defending player. CR 510.1c: anything
            -- else assigns no combat damage at all -- not damage addressed to an
            -- object that is not there, which would still be recorded in the CR
            -- 608.2i history even though marking it is a no-op.
            [] -> if trample then toDefender else pure ([], Map.empty)
            -- CR 510.1c / 702.19b: a single blocker with no trample -- or trample
            -- but no power past its threshold, and nothing else this step able to
            -- pay part of that threshold -- is forced: all onto the blocker. A
            -- single trample blocker WITH excess falls to the prompt arm.
            [blocker]
              | not trample || (not contested && power <= blockerThreshold gs attacker blocker) ->
                  pure ([damageEvent gs DamageKind.Combat attacker (Recipient.ToCreature blocker) power], Map.empty)
            blockers -> case Projection.controllerOf attacker gs of
              Nothing -> pure ([], Map.empty)
              -- CR 702.19b: the attacker's controller chooses how to assign the
              -- excess -- unless CR 702.22j inverts it.
              Just pid -> do
                -- CR 702.22j: with a banding creature among the blockers, the
                -- DEFENDING player divides the attacking creature's damage. The
                -- rule is stated as an exception to CR 510.1c alone, so it moves
                -- WHO is asked and nothing else -- the thresholds, the legality
                -- check and the reject-not-repair posture below are untouched.
                --
                -- CR 702.22k is the mirror, moving the division of a BLOCKING
                -- creature's damage to the active player. Its site is
                -- blockerChooser, below.
                --
                -- The defending player falls back to the attacker's controller
                -- rather than skipping the assignment. Unreachable: an attacker
                -- with no target at all assigns nothing and never gets here, and
                -- CR 702.19e's attacker -- which does get here, with its
                -- planeswalker off the battlefield -- reads the combat record's
                -- defending player, which CR 508.5's second sentence says is that
                -- planeswalker's controller. A battle that has left is the one
                -- shape that could reach the fallback (#1248); answering with the
                -- CR 510.1c default is the conservative reading there.
                let banded = any (\b -> Projection.hasKeyword Keyword.Banding b gs) blockers
                    chooser = if banded then Maybe.fromMaybe pid defending else pid
                    thresholdOf b = if trample then blockerThreshold gs attacker b else 0
                    blockerEntries = fmap (\b -> (Recipient.ToCreature b, thresholdOf b)) blockers
                divide chooser (blockerEntries <> (if trample then defenderEntries else []))

-- CR 702.22k: with a banding creature among the creatures it is blocking, the
-- ACTIVE player rather than the blocking creature's controller divides its
-- damage. Stated as an exception to CR 510.1d alone, so it moves WHO is asked and
-- nothing else. CR 702.22j is the mirror, on the attacking side.
--
-- The "bands with other [quality]" half of the rule is not read, and is part of
-- the unmodeled half Pawl.Types.Keyword's Banding note describes: no card in the
-- pool prints it.
blockerChooser :: GameState -> [ObjectId] -> PlayerId -> PlayerId
blockerChooser gs attackers controller =
  if any (\attacker -> Projection.hasKeyword Keyword.Banding attacker gs) attackers
    then GameState.activePlayer gs
    else controller

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
--
-- Keyed by the BLOCKER and not by the attacker, which is what rule 510.1d's third
-- and fourth sentences need: one creature blocking two attackers assigns its
-- damage divided among them, so the unit of assignment is the blocker. An action
-- and not a list for that division alone -- with one attacker blocked the rule
-- forces the whole assignment and asks nothing.
blockerAssignment :: GameState -> (ObjectId, Set.Set ObjectId) -> Game [DamageEvent.DamageEvent]
blockerAssignment gs (blocker, attackers) = case Projection.powerOf blocker gs of
  Nothing -> pure []
  Just p
    | p <= 0 -> pure []
    | otherwise ->
        let power = Integer.toNaturalSaturating p
         in case filter (\attacker -> onBattlefield attacker gs) (Set.toList attackers) of
              [] -> pure []
              [attacker] -> pure [damageEvent gs DamageKind.Combat blocker (Recipient.ToCreature attacker) power]
              blocked -> case Projection.controllerOf blocker gs of
                Nothing -> pure []
                -- Every threshold is 0: CR 510.1d's division is unconstrained,
                -- where CR 510.1c's has to clear lethal damage first. Trample is
                -- an attacker's keyword (CR 702.19b) and reaches nothing here.
                -- The thresholds divideAssignment answers with are dropped, and
                -- can be: tiersCleared over an all-zero map is vacuous, so a
                -- blocking creature's division has no gate for the step's other
                -- assignments to clear.
                Just pid ->
                  fmap fst
                    . divideAssignment
                      gs
                      blocker
                      power
                      (blockerChooser gs blocked pid)
                    $ fmap (\attacker -> (Recipient.ToCreature attacker, 0)) blocked

-- CR 510.1e / 702.19b: ask `chooser` to divide `source`'s `power` over `entries`,
-- and keep the answer only if it is well formed -- reject-not-repair (NOT the CR
-- 733 human-error rewind). An answer that does not total power, or that names a
-- recipient it was not offered, assigns nothing.
--
-- Only wellFormedAssignment here. The trample gates are the other half of CR
-- 510.1e and cannot be asked of one creature's answer, so gatherCombatDamage asks
-- them of the whole step and this function hands back the thresholds they are
-- stated over.
--
-- Shared by both assignment sides, so an attacker dividing among its blockers and
-- a blocker dividing among the creatures it blocks cannot come to disagree about
-- what an illegal answer does.
divideAssignment :: GameState -> ObjectId -> Natural -> PlayerId -> [(Recipient.Recipient, Natural)] -> Game ([DamageEvent.DamageEvent], Map.Map Recipient.Recipient Natural)
divideAssignment gs source power chooser entries = do
  let thresholds = Map.fromList entries
  chosen <- Game.choose (Prompt.AssignCombatDamage (Decide.deciderFor chooser gs) chooser source thresholds power)
  let toEvent (recipient, n) = damageEvent gs DamageKind.Combat source recipient n
  pure
    ( if wellFormedAssignment thresholds power chosen
        then fmap toEvent (filter (\(_, n) -> n > 0) (Map.toList chosen))
        else [],
      thresholds
    )

-- CR 510.2: gather all combat damage before applying any of it (simultaneity).
--
-- The blocking side is INVERTED out of Combat.blockers, which is keyed by
-- attacker: rule 510.1d assigns per blocking creature, and a blocker declared
-- against two attackers appears under both keys.
--
-- CR 510.1e: the attacking creatures' announcements are gathered FIRST and then
-- checked TOGETHER -- "the total damage assignment (not solely the damage
-- assignment of any individual attacking or blocking creature) is checked". That
-- is what makes CR 702.19b's and CR 702.19c's "damage from other creatures that's
-- being assigned during the same combat damage step" reach a gate at all: two
-- attackers blocked by one Palace Guard between them owe it one lethal bar, not
-- two, and a creature attacking a planeswalker beside a trampler pays down the
-- loyalty the trampler would otherwise have to cover alone.
gatherCombatDamage :: (ObjectId -> Bool) -> Game [DamageEvent.DamageEvent]
gatherCombatDamage assigns = do
  gs <- State.get
  let combat = GameState.combat gs
      attackers = filter (assigns . fst) (Map.toList (Combat.Type.attackers combat))
      blocking =
        Map.toList . Map.fromListWith Set.union $
          [ (blocker, Set.singleton attacker)
          | (attacker, blockers) <- Map.toList (Combat.Type.blockers combat),
            blocker <- Set.toList blockers,
            assigns blocker
          ]
  announced <- Monad.mapM (\a -> attackerAssignment gs (contestedAssignment gs attackers a) a) attackers
  fromBlockers <- Monad.mapM (blockerAssignment gs) blocking
  pure (settleAssignments announced <> concat fromBlockers)

-- Can any OTHER attacking creature assigning damage this step pay part of a bar
-- this one is gated by? The two ways CR 702.19b and CR 702.19c let that happen,
-- and the only two -- a creature assigns only to what is blocking it and to what
-- it is attacking (CR 510.1b-c):
--
--   * a blocker they share, whose lethal damage either may put in (CR 702.19b),
--   * the same attacked PLANESWALKER, whose loyalty either may pay down (CR
--     702.19c). Only a planeswalker: the other two attack targets carry threshold
--     0, so no share of them gates anything.
--
-- Power 0 or less is nobody's contribution (CR 510.1a), so such a creature leaves
-- the elision alone rather than turning a forced assignment into a prompt.
--
-- An over-approximation, and deliberately: the creature it names may announce
-- nothing toward the shared bar after all, leaving a division that had one legal
-- answer. That is the safe direction -- CR 510.1 announces the whole assignment at
-- once, so the choice is the player's to make even when it turns out forced.
contestedAssignment :: GameState -> [(ObjectId, AttackTarget)] -> (ObjectId, AttackTarget) -> Bool
contestedAssignment gs attackers (attacker, target) =
  let blockersFor oid = Set.filter (\b -> onBattlefield b gs) (Combat.blockersOf oid gs)
      mine = blockersFor attacker
      attackedPlaneswalker = case target of
        AttackTarget.OfPlaneswalker _ -> True
        AttackTarget.OfPlayer _ -> False
        AttackTarget.OfBattle _ -> False
      assigning oid = maybe False (> 0) (Projection.powerOf oid gs)
      shares (other, otherTarget) =
        other /= attacker
          && assigning other
          && ( (attackedPlaneswalker && otherTarget == target)
                 || not (Set.disjoint mine (blockersFor other))
             )
   in any shares attackers

-- CR 510.1e: keep the attacking creatures' announcements whose CR 702.19b /
-- 702.19c gates the whole step's damage clears.
--
-- A FIXPOINT and not one pass, because dropping one announcement lowers the
-- totals the others were checked against: an attacker that spilled past a blocker
-- only another attacker's damage made lethal must lose its spill too if that
-- other announcement is itself thrown out. Monotone -- fewer announcements clear
-- no more gates -- so each round either drops something or is the answer.
--
-- Reachable only through a broken interpreter, since an enforcing one never
-- announces a division the rules forbid, and it is the same reject-not-repair
-- posture divideAssignment takes: the loop is a fence rather than a proven
-- behaviour, no test being able to distinguish it from one pass.
settleAssignments :: [([DamageEvent.DamageEvent], Map.Map Recipient.Recipient Natural)] -> [DamageEvent.DamageEvent]
settleAssignments announced =
  let totalsOf events = Map.fromListWith (+) (fmap (\ev -> (DamageEvent.target ev, DamageEvent.amount ev)) events)
      everything = concatMap fst announced
      assigned = totalsOf everything
      -- CR 702.2c, read off the SAME round's events as the totals beside it: an
      -- announcement this round drops takes its deathtouch damage with it, so a
      -- gate only that damage cleared closes again.
      lethal = deathtouchedRecipients everything
      kept = filter (\(events, thresholds) -> tiersCleared thresholds assigned lethal (totalsOf events)) announced
   in if length kept == length announced
        then concatMap fst kept
        else settleAssignments kept

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
-- ToCreature, ToPlaneswalker, ToBattle and ToPlayer pass through untouched. Each
-- is produced either by combat (CR 510.1b-d) or by a CR 601.2c target chosen out
-- of a typed Pool, so CR 120.1a's gate has already been passed; re-asking it here
-- would be a second, later reading of that question, which is what CR 608.2b's
-- target re-validation is for and this is not.
--
-- Rule 120.1a's three object kinds are now all three classifiable, so a ToObject
-- naming none of them is CR 120.1a's "can't" and nothing else.
--
-- The order of the three tests settles only WHICH tag a permanent with more than
-- one of those card types is given, never what damage to it does: damagedCardTypes
-- below reads the recipient's projected card types as the damage is applied, so a
-- permanent that is both a creature and a planeswalker gets CR 120.3c and CR
-- 120.3e whichever arm classified it.
damageRecipient :: GameState -> Recipient.Recipient -> Maybe Recipient.Recipient
damageRecipient gs recipient = case recipient of
  Recipient.ToPlayer _ -> Just recipient
  Recipient.ToCreature _ -> Just recipient
  Recipient.ToPlaneswalker _ -> Just recipient
  Recipient.ToBattle _ -> Just recipient
  Recipient.ToObject oid
    | Projection.isCreatureOf oid gs -> Just (Recipient.ToCreature oid)
    | Projection.isPlaneswalkerOf oid gs -> Just (Recipient.ToPlaneswalker oid)
    | Projection.isBattleOf oid gs -> Just (Recipient.ToBattle oid)
    | otherwise -> Nothing

-- CR 120.3: "damage may have one or more of the following results, depending on
-- ... the characteristics of the damage's recipient". ONE OR MORE, and the
-- characteristics are the recipient's -- so the results damage to a permanent has
-- are keyed to the set of CR 120.1a card types it has, not to a single choice
-- among them. This is that set, narrowed to the three card types CR 120.1a admits
-- and CR 120.3c/120.3e/120.3h give results to.
--
-- A permanent really can hold two of them at once: Liquimetal Coating makes Jace
-- Beleren an artifact "in addition to its other types" and March of the Machines
-- then makes that noncreature artifact an artifact creature, both under CR
-- 205.1b's retention clause, leaving a 3/3 artifact creature planeswalker that is
-- owed CR 120.3c AND CR 120.3e off one damage event. Pawl.DamageSpec's
-- CreatureAndPlaneswalker group is the proof.
--
-- The recipient's own TAG is unioned in rather than replaced by the projection,
-- and the union is what keeps this from ever taking a result away. The tag is the
-- classification made where the recipient was built -- CR 510.1b's combat
-- recipient, CR 601.2c's chosen target -- and it is the only reading left for a
-- permanent that is no longer there to be projected, which is CR 608.2h's last
-- known information. The projection is what CR 120.3 actually asks for and what
-- one tag cannot express.
--
-- Empty for a player, who is CR 120.3a/120.3b's business and has no card types at
-- all, and for a ToObject naming nothing (CR 608.2h) or naming a permanent that
-- is none of the three (CR 120.1a's "can't").
damagedCardTypes :: GameState -> Recipient.Recipient -> Set.Set CardType.CardType
damagedCardTypes gs recipient =
  let tagged = case recipient of
        Recipient.ToCreature _ -> Set.singleton CardType.Creature
        Recipient.ToPlaneswalker _ -> Set.singleton CardType.Planeswalker
        Recipient.ToBattle _ -> Set.singleton CardType.Battle
        Recipient.ToObject _ -> Set.empty
        Recipient.ToPlayer _ -> Set.empty
      projected = case Recipient.objectOf recipient of
        Nothing -> Set.empty
        Just oid -> Set.intersection damageable (Projection.cardTypesOf oid gs)
      damageable = Set.fromList [CardType.Battle, CardType.Creature, CardType.Planeswalker]
   in Set.union tagged projected

-- CR 120.3: carry out a batch of damage events' results -- mark damage on
-- creatures (CR 120.3e), drain life from players (CR 120.3a), take loyalty and
-- defense counters off (CR 120.3c, CR 120.3h), place the counters infect, wither
-- and toxic cause (CR 120.3b, CR 120.3d, CR 120.3g) -- AND record each event into
-- GameState.events. The change-and-emit funnel for combat's two waves and
-- resolving effects alike.
--
-- CR 615 / 616: EACH event in the batch runs its OWN CR 616.1 loop, and the
-- survivors are applied together. Simultaneity is preserved as a SCHEDULING
-- property; the loop's unit stays one event, uniform with the other five
-- classes, which is what CR 614.5 and CR 615.10 both describe.
--
-- The whole batch goes through Event.resolveDamageBatch rather than
-- through resolveDamage one event at a time, and CR 615.7 is one reason: a
-- prevent-the-next-N shield (Mending Hands) is ONE resource allocated across
-- several simultaneous events, and the shielded side chooses which damage it
-- prevents. Those loops still run sequentially and the shield is still spent by
-- whichever runs first -- what changed is that the shielded side, not the batch's
-- gather order, says which that is.
--
-- CR 616.1's APNAP clause is the other: the batch is the only place two players
-- can owe a replacement choice at the same time, so the batch is where turn
-- order over those choices can be honoured.
--
-- The batch is also the unit CR 615.13 counts preventions in, which is the other
-- half of what resolveDamageBatch answers: one Prevention per prevention effect
-- that applied to this batch, carrying the total it prevented.
--
-- CR 120.4b and CR 120.4c only. CR 120.4a's excess-damage rewrite does not happen
-- here or anywhere (#980).
applyDamage :: [DamageEvent.DamageEvent] -> Game ()
applyDamage events = do
  (survivors, prevented) <- Event.resolveDamageBatch events
  -- The state the whole batch is read against: recipients are classified off it
  -- (damagedCardTypes), the commander tally is asked of it, and `counterResults`
  -- reads each source's controller out of it. CR 510.2's simultaneity, which no
  -- event in the batch may disturb for the next.
  board <- State.get
  let -- CR 120.3e: mark the damage. CR 120.3d's alternative -- the -1/-1 counters
      -- a wither and/or infect source's damage causes instead -- is in
      -- `counterResults` below rather than here, since it goes through
      -- Event.putCounters, CR 122.6's funnel, and this fold is pure.
      --
      -- The disjunction is CR 120.3d's "wither and/or infect" verbatim, and its
      -- scope is this creature arm ALONE: CR 120.3a's life-loss exception names
      -- infect and not wither, so markOne's ToPlayer arm below stays infect-only.
      markCreature ev oid g =
        if DamageEvent.dealtByInfect ev || DamageEvent.dealtByWither ev
          then g
          else
            let hurt obj = obj {Object.damage = Object.damage obj + DamageEvent.amount ev}
             in g {GameState.objects = Map.adjust hurt oid (GameState.objects g)}
      -- CR 306.8 / CR 120.3c and CR 310.6 / CR 120.3h: damage dealt to a
      -- planeswalker removes that many LOYALTY counters, and damage dealt to a
      -- battle that many DEFENSE counters. One function, because the two rules
      -- differ in nothing but the counter kind and read the same three ways:
      -- removed directly rather than through a "would remove counters"
      -- sub-replacement, there being no such thing -- CR 614.16 scales counters an
      -- effect PUTS on, and nothing in CR 614 replaces a removal -- floored at 0
      -- rather than wrapped, because Object.counters is Natural while CR 306.5c
      -- makes loyalty and CR 122.1g defense the COUNT of those counters, and
      -- destroying nothing (CR 120.5): CR 704.5i and CR 704.5v are what read the 0.
      removeCounters kind ev oid g =
        let have obj = Map.findWithDefault 0 kind (Object.counters obj)
            strip obj =
              obj
                { Object.counters =
                    Map.insert
                      kind
                      (Natural.minusSaturating (have obj) (DamageEvent.amount ev))
                      (Object.counters obj)
                }
         in g {GameState.objects = Map.adjust strip oid (GameState.objects g)}
      -- CR 120.3's "one or more of the following results", read off
      -- damagedCardTypes: every result the recipient's card types earn it applies,
      -- rather than the first one that matches. A permanent that is both a creature
      -- and a planeswalker takes CR 120.3e's mark AND loses CR 120.3c's loyalty
      -- counters from one event.
      --
      -- `board` is the state the batch is applied AGAINST and not the accumulator,
      -- so every event in one CR 120.4b batch classifies its recipient off the same
      -- board -- CR 510.2's simultaneity, which an earlier event in the fold must
      -- not disturb.
      onPermanent ev oid g =
        let results =
              [ (CardType.Creature, markCreature ev oid),
                (CardType.Planeswalker, removeCounters CounterKind.Loyalty ev oid),
                (CardType.Battle, removeCounters CounterKind.Defense ev oid)
              ]
            apply h (cardType, result) = if Set.member cardType (damagedTypesOf ev) then result h else h
         in List.foldl' apply g results
      -- onPermanent's classification, hoisted so `counterResults` below shares it:
      -- CR 120.3d's counters go to a CREATURE recipient, the same card type CR
      -- 120.3e's mark answers to, so the two readings of "is this a creature" must
      -- be one reading.
      damagedTypesOf ev = damagedCardTypes board (DamageEvent.target ev)
      markOne g ev = case DamageEvent.target ev of
        -- Every object-shaped recipient goes through one arm, INCLUDING
        -- Recipient.ToObject: CR 120.1a's three card types are what decide the
        -- results, and onPermanent asks the projection for them rather than
        -- trusting the tag. Combat never builds a ToObject (CR 510.1b-d name a
        -- creature, a player, a planeswalker or a battle) and the one producer that
        -- can -- Resolve's DealDamage arm, naming a permanent generically -- runs
        -- every recipient through damageRecipient above first, so this arm is
        -- defensive; what it must not be is a silent drop.
        Recipient.ToCreature oid -> onPermanent ev oid g
        Recipient.ToPlaneswalker oid -> onPermanent ev oid g
        Recipient.ToBattle oid -> onPermanent ev oid g
        Recipient.ToObject oid -> onPermanent ev oid g
        Recipient.ToPlayer pid ->
          -- CR 120.3a: the life loss, and only it. Both poison diversions -- CR
          -- 120.3b / 702.90b's infect, which replaces the life loss, and CR 120.3g
          -- / 702.164c's toxic, which adds poison alongside it -- are in
          -- `counterResults` below, since they go through
          -- Event.putPlayerCounters, CR 122.6's player funnel. What stays here is
          -- WHETHER life is lost, which is the half infect replaces.
          --
          -- WITHER IS ABSENT ON PURPOSE. CR 120.3d pairs it with infect for a
          -- creature recipient only; CR 120.3a's exception names infect alone, so
          -- a wither source drains a player's life like any other.
          if DamageEvent.dealtByInfect ev
            then g
            else
              let drain player = player {Player.life = Player.life player - toInteger (DamageEvent.amount ev)}
               in g {GameState.players = Map.adjust drain pid (GameState.players g)}
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
      -- CR 903.10a: the running tally of COMBAT damage each commander has dealt
      -- each player, kept under the commander's OWNER (Player.commanderDamage).
      --
      -- A third pass rather than a line inside markOne, for `gainOne`'s reason:
      -- the tally is not one of CR 120.3's results at all -- it is a record CR
      -- 903.10a keeps beside them -- so no arm of markOne can turn it off.
      --
      -- Asked of `board`, the pre-batch state the rest of applyDamage classifies
      -- against, so every event in one batch reads the same answer to "was the
      -- source a commander" -- CR 510.2's simultaneity, which an earlier event in
      -- the fold must not disturb.
      --
      -- Over `survivors`, so prevented and replaced damage never counts: rule
      -- 903.10a counts damage DEALT.
      --
      -- COMBAT damage only, which is rule 903.10a's own word and why the kind is
      -- tested rather than assumed: Pawl.Engine.Resolve's DealDamage arm builds
      -- Noncombat events from the very same sources.
      --
      -- Players only, so this is a walk of its own rather than a row in
      -- onPermanent's `results` list: that list is keyed by CardType and a player
      -- has none (damagedCardTypes answers empty for one).
      tallyOne g ev = case DamageEvent.target ev of
        Recipient.ToPlayer pid
          | DamageEvent.kind ev == DamageKind.Combat ->
              case Commander.commanderOwnerOf (DamageEvent.source ev) board of
                Nothing -> g
                Just owner ->
                  let count player =
                        player
                          { Player.commanderDamage =
                              Map.insertWith (+) owner (DamageEvent.amount ev) (Player.commanderDamage player)
                          }
                   in g {GameState.players = Map.adjust count pid (GameState.players g)}
        _ -> g
      -- CR 119.2: damage dealt to a player CAUSES that player to lose that much
      -- life, so the loss is recorded here and never by Pawl.Engine.Resolve's
      -- LoseLife arm, which no damage runs through.
      --
      -- A separate pass rather than a line inside markOne, for the reason the
      -- recording block below gives: markOne runs BEFORE either record is
      -- appended, so a life loss written there would be logged ahead of the
      -- damage that caused it and would move CR 603.3a's control sample onto a
      -- pre-lifelink board.
      --
      -- Only where life was actually lost. CR 120.3b's infect diversion replaces
      -- the life loss with poison counters, and a 0-damage event loses nothing --
      -- neither is a life loss event for CR 702.179d to see. Only players: CR
      -- 120.3c and CR 120.3d take a permanent's damage somewhere else entirely.
      --
      -- Infect and NOT wither, which is CR 120.3a's own wording. Pinned by
      -- DamageSpec's Wither group, which asserts the recorded LifeLost event and
      -- not merely the total.
      lifeLostBy ev = case DamageEvent.target ev of
        Recipient.ToPlayer pid
          | not (DamageEvent.dealtByInfect ev),
            DamageEvent.amount ev > 0 ->
              [GameEvent.LifeLost (LifeChange.MkLifeChange pid (DamageEvent.amount ev))]
        _ -> []
      -- CR 120.3f's gain, recorded where `gainOne` above performs it, so that
      -- "whenever you gain life" sees lifelink (CR 702.15b) and not only an
      -- effect that says the words.
      --
      -- ONE record per damage event, never per player, which is CR 702.15e in as
      -- many words: "if multiple sources with lifelink deal damage at the same
      -- time, they cause separate life gain events". Summing them first would be
      -- one event, and a Pridemate would get one counter where it is owed two.
      --
      -- Whom the damage was dealt to is none of this function's business, exactly
      -- as it is none of `gainOne`'s: CR 702.15b hangs the gain off the SOURCE, so
      -- damage to a creature or a planeswalker gains life just as damage to a
      -- player does. `lifeLostBy` above scopes to a player recipient because CR
      -- 120.3a does; this one must not.
      --
      -- CR 119.9's zero guard, stated HERE rather than left to the callers, and
      -- the same posture `lifeLostBy` above takes. No producer in the pool builds
      -- a 0-amount event today -- CR 510.1a makes `attackerAssignment` drop a
      -- creature assigning 0 or less, and Resolve's DealDamage arm guards its own
      -- quantity -- so this restates the rule at the site that writes the record
      -- instead of resting on an invariant two modules away.
      lifeGainedBy ev = case DamageEvent.dealtByLifelink ev of
        Just pid | DamageEvent.amount ev > 0 -> [GameEvent.LifeGained (LifeChange.MkLifeChange pid (DamageEvent.amount ev))]
        _ -> []
      -- CR 310.6's counter removal, recorded so CR 310.11b's "when the last
      -- defense counter is removed" has an event to fire off.
      --
      -- ONE record per BATTLE and not one per damage event, which is CR 510.2's
      -- simultaneity taken seriously: two unblocked attackers that between them
      -- take a defense-5 battle to 0 removed its last defense counter ONCE, so the
      -- Siege ability triggers once. A per-event record would have neither event
      -- reach 0 and the trigger would never fire at all.
      --
      -- Read as a BEFORE/AFTER pair off the two boards rather than summed out of
      -- the events, so the record describes what actually came off: the floor in
      -- removeCounters above means 4 damage to a defense-1 battle removes one counter,
      -- not four.
      --
      -- Every PERMANENT a surviving event named, not only the ones tagged
      -- ToBattle: whether a defense counter actually came off is what the
      -- before/after pair below answers, so this need not classify the recipient a
      -- second time -- and a permanent that is a battle under some other tag (CR
      -- 120.3h alongside CR 120.3e) is therefore not missed.
      battleHit ev = Recipient.objectOf (DamageEvent.target ev)
      removalOn before after oid =
        let was = Battle.defenseOn oid before
            now = Battle.defenseOn oid after
         in [GameEvent.CountersRemoved oid CounterKind.Defense was now | was > now]
      removalsBetween before after =
        concatMap (removalOn before after) (List.nub (Maybe.mapMaybe battleHit survivors))
      -- CR 120.3b / 702.90b, CR 120.3d / 702.90c / 702.80a and CR 120.3g /
      -- 702.164c: the counters a damage event CAUSES, placed through
      -- Event.putCounters and Event.putPlayerCounters -- CR 122.6's two funnels --
      -- so each placement runs its own CR 616.1 loop and a counter replacement
      -- reaches it (Vorinclex, Monstrous Raider).
      --
      -- A monadic pass of its own rather than an arm of markOne, because those
      -- funnels are Game actions and the fold above is pure. That is the whole
      -- reason for the split: the three rules are as much "the damage's results"
      -- as the mark and the life loss are.
      --
      -- The PUTTER is the damage SOURCE's controller, which all three rules name
      -- outright: "that source's controller" in CR 120.3b and CR 120.3d, "that
      -- creature's controller" in CR 120.3g, whose source is that creature. Read
      -- through controllerWithLastKnown, the reader damageEvent's riders use, since
      -- CR 608.2h's last known information is all that is left of a source that
      -- killed itself to deal the damage (Ghitu Fire-Eater); Nothing survives only
      -- for an id nothing was ever filed under, which no producer builds.
      --
      -- CounterCause.ByRule and not ByEffect: rule 120.3's results are dictated by
      -- the rules, not by "the effect of a resolving spell or ability" nor by
      -- "another replacement or prevention effect", which are CR 614.16's two
      -- admitted causes. So a clause naming a player (Vorinclex) reaches these,
      -- and rule 614.16's own subject (Doubling Season) does not -- the same split
      -- CR 714.3c's lore counter falls on.
      --
      -- Read against `board` for both the putter and the recipient's card types,
      -- so every event in the batch answers CR 510.2's questions off one state.
      --
      -- Pawl.ReplacementSpec's "Counters damage causes" group is the proof, on both
      -- axes: that a replacement reaches these at all, and that it is the SOURCE's
      -- controller and not the recipient who is putting them.
      counterResults ev =
        let poison =
              -- CR 120.3b's poison REPLACES the life loss and CR 120.3g's is IN
              -- ADDITION to the damage's other results, so a source with both
              -- gives the sum. Toxic is scoped to COMBAT damage, so a noncombat
              -- event's captured value is ignored.
              --
              -- ONE placement and not two, because CR 122.6 knows only how many
              -- counters of a kind are being put: two calls would run two CR 616.1
              -- loops over one player's poison and let a one-shot replacement be
              -- spent twice. No printing has both keywords anyway (DamageSpec
              -- hand-builds the event).
              (if DamageEvent.dealtByInfect ev then DamageEvent.amount ev else 0)
                + case DamageEvent.kind ev of
                  DamageKind.Combat -> DamageEvent.dealtByToxic ev
                  DamageKind.Noncombat -> 0
            -- CR 120.3d's "wither and/or infect", the same disjunction markCreature
            -- reads, and gated on the recipient being a CREATURE by the same
            -- classification -- damage to a planeswalker by an infect source is CR
            -- 120.3c's business and nothing else's.
            minusOnes =
              if (DamageEvent.dealtByInfect ev || DamageEvent.dealtByWither ev)
                && Set.member CardType.Creature (damagedTypesOf ev)
                then DamageEvent.amount ev
                else 0
            -- Nothing proposed for nothing to put on. CR 122.6 has no zero
            -- placement to replace, and raising one would let a Uses.Once counter
            -- replacement be spent on an event that never happened -- a fence
            -- rather than an observable, no printing in the pool having a one-shot
            -- counter replacement to spend.
            place n go = Monad.when (n > 0) (Monad.void (go n))
         in case Projection.controllerWithLastKnown (DamageEvent.source ev) board of
              Nothing -> pure ()
              Just putter ->
                let cause = CounterCause.ByRule putter
                    onObject oid = place minusOnes (Event.putCounters cause oid CounterKind.MinusOneMinusOne)
                 in case DamageEvent.target ev of
                      -- markOne's arms, one for one: every object-shaped recipient
                      -- routes through onObject, whose own gate is the card types.
                      -- Written out rather than left to a wildcard, so a sixth
                      -- Recipient cannot join the object arms by default.
                      Recipient.ToCreature oid -> onObject oid
                      Recipient.ToPlaneswalker oid -> onObject oid
                      Recipient.ToBattle oid -> onObject oid
                      Recipient.ToObject oid -> onObject oid
                      Recipient.ToPlayer pid -> place poison (Event.putPlayerCounters cause pid PlayerCounterKind.Poison)
  -- CR 608.2i: each surviving event is RECORDED, not enqueued. Sba consumes by
  -- bumping GameState.damageScannedThrough; the record survives the check.
  --
  -- CR 615.13's preventions are recorded FIRST, which is the order they happened
  -- in: a prevention is applied inside the CR 616.1 loop, which settles what the
  -- event will be BEFORE the surviving damage is dealt. Both records land inside
  -- one CR 117.5 boundary, so the two kinds of trigger are gathered together
  -- either way and CR 603.3b lets their controller order them; what this fixes is
  -- the canonical order the prompt indexes into.
  State.modify'
    ( \gs ->
        let marked = List.foldl' markOne gs survivors
            gained = List.foldl' gainOne marked survivors
            tallied = List.foldl' tallyOne gained survivors
            noted = List.foldl' (\g p -> Event.recordEvent (GameEvent.DamagePrevented (DamagePrevented.MkDamagePrevented (Prevention.recipient p) (Prevention.amount p))) g) tallied prevented
            dealt = List.foldl' (\g ev -> Event.recordEvent (GameEvent.DamageDealt ev) g) noted survivors
         in -- CR 119.2's life loss and CR 120.3f's life gain are recorded AFTER
            -- the damage that caused them, which is the same reasoning the
            -- prevention/damage order above follows: both land inside one CR 117.5
            -- boundary, so the triggers are gathered together either way, and this
            -- fixes only the canonical order. Simultaneous with the damage under
            -- CR 119.2 and CR 120.3f's "in addition", and logged after it because
            -- a cause reads before its consequence.
            --
            -- Per SURVIVOR rather than two passes, so one damage event's loss and
            -- gain sit together: a lifelink attacker's two records describe a
            -- single event, and CR 702.15e already makes each source's gain its
            -- own entry.
            --
            -- CR 310.6's counter removals join them, after the damage and for the
            -- same reason: they are a RESULT of it (CR 120.3h), so the cause reads
            -- first. `marked` is the board markOne left, which is what makes the
            -- pair the removal that actually happened.
            List.foldl'
              (flip Event.recordEvent)
              dealt
              (concatMap (\ev -> lifeLostBy ev <> lifeGainedBy ev) survivors <> removalsBetween board marked)
    )
  -- CR 120.3's counter results, run AFTER the records above for `lifeLostBy`'s
  -- reason: putCounters records CR 122.6's own CountersPut, and a cause reads
  -- before its consequence. Both land inside one CR 117.5 boundary, so what this
  -- settles is the canonical order and not which triggers are gathered.
  --
  -- The placements are simultaneous with the damage under CR 120.3, so nothing may
  -- observe the board between the fold above and this pass -- which is why it
  -- reads `board` rather than the state it is running against.
  Monad.mapM_ counterResults survivors
  -- CR 615.5: "the rest of the effect takes place immediately afterward". This
  -- module cannot run it -- Pawl.Engine.Resolve depends on this one -- so the
  -- applications that carry an additional effect are QUEUED and Resolve drains
  -- them, which both of this function's callers do before anything else can
  -- observe the board. Only the ones with a rider, so the queue is empty on
  -- every board but the handful this rule reaches.
  --
  -- Appended AFTER the records above, which is the order the two happened in:
  -- the CR 608.2i record is of the prevention itself and the rider is the "rest
  -- of the effect". Nothing is inspected here; the rider is an opaque payload
  -- copied from Prevention to the queue.
  State.modify' $ \gs ->
    gs {GameState.pendingPreventionRiders = GameState.pendingPreventionRiders gs <> Seq.fromList (filter (Maybe.isJust . Prevention.rider) prevented)}

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

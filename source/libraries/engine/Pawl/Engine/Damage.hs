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
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.ExcessDestination as ExcessDestination
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.LifeLossCause as LifeLossCause
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
  -- Unreachable, and the same for every arm this module writes for it: CR
  -- 406.4's pile is a candidate at CR 601.2c that Pawl.Engine.Target.drawFromPiles
  -- replaces before a target is recorded, so no damage ever names one.
  Recipient.ToPile _ -> 0

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
        Recipient.ToPile _ -> []
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
  let lethal = lethalRemaining gs blocker
   in if lethal > 0 && Projection.hasKeyword Keyword.Deathtouch attacker gs
        then 1
        else lethal

-- CR 120.6: how much more damage this creature can take before "the total damage
-- marked on [it] is greater than or equal to its toughness" -- toughness minus
-- what is already marked, floored at 0. Read through the projection, the same
-- way the CR 704.5g SBA reads it.
--
-- The bar both of its callers START from, and neither of them is this alone,
-- because the two rules that mention deathtouch here do not agree. CR 702.2c
-- makes any nonzero COMBAT assignment by a deathtouch source lethal, so
-- blockerThreshold above asks for 1 -- and 0 from a creature already at its bar,
-- which is the reading that lets a trampler spill everything past it. CR 120.4a
-- instead says "any amount of damage greater than 1 is EXCESS damage if the
-- source dealing that damage to a creature has deathtouch", unconditionally, so
-- excessThreshold below asks for a flat 1 there. Each caller applies its own
-- rather than sharing one, and this function is what they share.
lethalRemaining :: GameState -> ObjectId -> Natural
lethalRemaining gs oid =
  let marked = maybe 0 Object.damage (Game.lookupObject oid gs)
   in case Projection.toughnessOf oid gs of
        Nothing -> 0
        Just t -> Integer.toNaturalSaturating (t - toInteger marked)

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
-- The defending player is Defender.playerOf's answer for the very target the
-- attacker is still recorded against, and never the head of
-- Defender.defendingPlayers: CR 802.2a resolves "a defending player" per
-- attacking creature from what that creature is attacking, which at three or more
-- seats is a different player from the group's first. CR 508.5's second sentence
-- is why it is still answerable once the planeswalker is gone -- the controller
-- lookup is Projection.controllerWithLastKnown, CR 608.2h's last known
-- information, the same reader attackerAssignment's `defending` passes so the CR
-- 702.19c threshold map and this recipient cannot name different seats.
-- Pawl.CombatEffectSpec's "CR 802.2a the removed planeswalker's trampler drains
-- ITS controller" is the three-seat board that proves it.
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
      -- CR 506.4c: this attacker's target has not ALREADY been removed from
      -- combat. Asked alongside the live derivation and not instead of it,
      -- because rule 506.4 names events -- see Pawl.Types.Combat's
      -- attackingNothing.
      notNotedAsAttackingNothing =
        Set.notMember attacker (Combat.Type.attackingNothing (GameState.combat gs))
   in case target of
        AttackTarget.OfPlayer defender -> stillPlaying defender
        AttackTarget.OfPlaneswalker oid
          | notNotedAsAttackingNothing,
            Combat.stillAttacked oid gs ->
              Just (Recipient.ToPlaneswalker oid)
          | Projection.hasKeyword Keyword.TrampleOverPlaneswalkers attacker gs ->
              stillPlaying =<< Defender.defenderOfAttack Projection.controllerWithLastKnown attacker target gs
          | otherwise -> Nothing
        -- CR 310.5 / 506.4, the planeswalker arm's twin: a battle that has left the
        -- battlefield is removed from combat and stops being attacked, so CR 510.1b
        -- gives the creature nothing to assign to. No CR 702.19e for a battle: that
        -- rule names a planeswalker only.
        AttackTarget.OfBattle oid ->
          if notNotedAsAttackingNothing && Combat.stillAttackedBattle oid gs
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
            -- CR 508.5 / CR 310.9d, shared with the landwalk reading in
            -- Defender.defenderOfAttack so the two cannot drift. Read once, for CR
            -- 702.19c's third recipient below and for CR 702.22j's chooser.
            defending = Defender.defenderOfAttack Projection.controllerWithLastKnown attacker target gs
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
                -- planeswalker's controller. An attacker whose BATTLE has left
                -- reads that same record (CR 506.4c / 506.2), so the chooser is
                -- CR 702.22j's defending player there too. What is left for the
                -- fallback is a battle still on the battlefield designating
                -- nobody, which CR 310.11's repair does not leave standing.
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
      -- CR 703.4k / CR 802.5: each player in APNAP order announces how each
      -- attacking or blocking creature THEY CONTROL assigns its combat damage.
      -- Partitioned by controller rather than by role: attackers-then-blockers
      -- is APNAP order only while every blocker belongs to one nonactive
      -- player, which is what CR 802.2's several defending players break.
      --
      -- Rule 703.4k gives no order WITHIN a player, so each seat's attackers
      -- are asked before its blockers, which is the order the two lists were
      -- already built in.
      --
      -- Anything whose controller is not a seat in the game is announced LAST
      -- rather than dropped: the rule names players, and a creature CR 800.4a
      -- has not removed still assigns its damage.
      seats = Game.apnapOrder gs
      controllerOfEntry entry = Projection.controllerOf (fst entry) gs
      order owned =
        fmap (\pid -> filter (\entry -> controllerOfEntry entry == Just pid) owned) seats
          <> [filter (Maybe.maybe True (\pid -> List.notElem pid seats) . controllerOfEntry) owned]
  waves <-
    Monad.mapM
      ( \(mine, theirs) -> do
          a <- Monad.mapM (\x -> attackerAssignment gs (contestedAssignment gs attackers x) x) mine
          b <- Monad.mapM (blockerAssignment gs) theirs
          pure (a, b)
      )
      (zip (order attackers) (order blocking))
  pure (settleAssignments (concatMap fst waves) <> concatMap (concat . snd) waves)

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
  Recipient.ToPile _ -> Nothing

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
        Recipient.ToPile _ -> Set.empty
      projected = case Recipient.objectOf recipient of
        Nothing -> Set.empty
        Just oid -> Set.intersection damageable (Projection.cardTypesOf oid gs)
      damageable = Set.fromList [CardType.Battle, CardType.Creature, CardType.Planeswalker]
   in Set.union tagged projected

-- CR 120.4a: the bar a permanent's own characteristics set, below which none of
-- an effect's damage to it is EXCESS. One number per recipient, so redirectExcess
-- below is arithmetic on the event and never a second classification of the
-- permanent.
--
-- One entry per CR 120.1a card type the recipient HAS, read off damagedCardTypes
-- so this and CR 120.3's results agree about what the permanent is:
--
--   * a creature's bar is CR 120.6's lethal damage, "taking into account damage
--     already marked" (lethalRemaining) -- or a flat 1 from a source with
--     deathtouch, which is 120.4a's own sentence and not CR 702.2c's (see
--     lethalRemaining's note on the difference).
--   * a planeswalker's is its loyalty, which is its loyalty counters (CR 306.5c).
--   * a battle's is its defense, which is its defense counters (CR 310.4c).
--
-- The MINIMUM of those, because the rule takes the maximum of the other side:
-- "if the first permanent has multiple card types ... the excess damage is the
-- greatest of the calculated amounts for each of the card types it has", and the
-- greatest excess is what the lowest bar leaves. Only reachable for a permanent
-- that is more than one of the three at once, which takes Liquimetal Coating and
-- March of the Machines to build (Pawl.DamageSpec's ExcessDamage group casts
-- Flame Spill at that Jace Beleren); one card type is the ordinary case.
--
-- Nothing when the recipient has none of them: a player (CR 120.3a's business,
-- and not a permanent at all) or an object that is no longer there to project.
-- Nothing means no rewrite, which is the do-as-much-as-you-can reading -- the
-- damage is dealt as the effect otherwise says.
--
-- "Damage from other sources that would be dealt at the same time" is not read.
-- Every event in one of these batches comes from the SAME source, since
-- Pawl.Engine.Resolve's DealDamage arm resolves one dealer for the whole
-- instruction, and pawl deals no two effects' damage simultaneously.
excessThreshold :: GameState -> ObjectId -> Recipient.Recipient -> Maybe Natural
excessThreshold gs source recipient = do
  oid <- Recipient.objectOf recipient
  let types = damagedCardTypes gs recipient
      bars =
        [ bar
        | (cardType, bar) <-
            [ ( CardType.Creature,
                if Projection.hasKeyword Keyword.Deathtouch source gs
                  then 1
                  else lethalRemaining gs oid
              ),
              (CardType.Planeswalker, Cost.loyaltyCountersOn oid gs),
              (CardType.Battle, Battle.defenseOn oid gs)
            ],
          Set.member cardType types
        ]
  Monad.guard (not (null bars))
  pure (minimum bars)

-- CR 120.4's damage EVENT, whose granularity is one source, one recipient, one
-- moment: the events of ONE instruction that name the same recipient are summed
-- into one. Char's "4 damage to any target and 2 damage to you", aimed at its own
-- caster, deals that caster one event of 6.
--
-- Nothing in the CR individuates simultaneous damage more finely than by source.
-- CR 615.7 puts its allocation question only to damage dealt "by two or more
-- applicable sources at the same time"; CR 120.4a computes excess against "damage
-- from other sources that would be dealt at the same time"; CR 120.9 scopes a
-- trigger's "damage dealt" to the sources it names. CR 701.14c is the rule's own
-- worked instance of this collapse: a creature that fights itself "deals damage
-- to itself equal to twice its power" -- one blow, not its power twice.
--
-- Keyed on the whole event but its amount, so two events merge only when their
-- source, their recipient, their kind and every CR 702 deal-time rider agree --
-- the source among them, so this could not merge two dealers' damage even if it
-- were handed some. The first occurrence keeps its place: the batch's order is
-- the order its events are offered in when CR 615.7 asks which of them a shield
-- prevents.
--
-- Scoped to ONE instruction, which has exactly one dealer (CR 120.1) and one
-- moment (CR 608.2f); Pawl.Engine.Resolve's DealDamage arm is the only caller.
-- Combat must NOT come through here -- two attacking creatures hitting one player
-- are two sources, so CR 615.10's floor applies to each -- and neither must a
-- fight, whose two blows have different dealers (CR 701.14a).
--
-- Observable, and Pawl.ReplacementSpec's Ajani Steadfast emblem case is the
-- proof: CR 615.10's floor applies once per event, so 4 and 2 at one recipient
-- deal 1 as one event and would deal 2 as two.
oneEventPerRecipient :: [DamageEvent.DamageEvent] -> [DamageEvent.DamageEvent]
oneEventPerRecipient events =
  let key event = event {DamageEvent.amount = 0}
      total event = sum (fmap DamageEvent.amount (filter (\other -> key other == key event) events))
   in fmap (\event -> event {DamageEvent.amount = total event}) (List.nubBy (\one two -> key one == key two) events)

-- CR 120.4a, the FIRST part of CR 120.4's four-part sequence: "if an effect
-- that's causing damage to be dealt states that excess damage that would be
-- dealt to a permanent is dealt to another permanent or player instead, the
-- damage event is modified accordingly".
--
-- The events go in raw and come out rewritten, ahead of applyDamage -- so the CR
-- 120.4b replacement and prevention loop sees the split events rather than the
-- original one, which is what the ordering of those two rules says and is
-- observable: a prevention effect protecting the redirected player applies to
-- the redirected half alone.
--
-- Nothing -- the effect stated no destination -- means no rewrite at all, and the
-- whole amount stays where the effect aimed it. Combat never reaches here either:
-- CR 120.4a's subject is an effect, and Pawl.Engine.Resolve's DealDamage arm is
-- the only caller.
--
-- The redirected half is built through damageEvent, the one place a damage event
-- is made, so it carries the same source and the same deal-time riders (CR
-- 702.2e, CR 702.15c): a lifelink source redirecting its excess to a player
-- still gains its controller that life.
redirectExcess :: GameState -> Maybe ExcessDestination.ExcessDestination -> [DamageEvent.DamageEvent] -> [DamageEvent.DamageEvent]
redirectExcess gs destination events = case destination of
  Nothing -> events
  -- "Excess damage is dealt to that creature's controller instead", read off the
  -- damaged permanent rather than off the card, which names no one.
  Just ExcessDestination.ToRecipientController -> concatMap (splitExcess gs) events

-- One event, split into what the permanent can take and what its controller
-- takes instead. Unsplit -- the singleton -- whenever any part of the rewrite
-- has no answer: a recipient with no bar (excessThreshold), no excess to move,
-- or a permanent whose controller cannot be read.
--
-- The permanent's half DROPS when its bar is 0, rather than being dealt as a 0:
-- CR 120.8, "if a source would deal 0 damage, it does not deal damage at all",
-- which the sibling reading in Resolve's DealDamage arm already takes for a
-- quantity that evaluates to 0. An already-lethal creature therefore sends the
-- whole amount on and takes no second event.
splitExcess :: GameState -> DamageEvent.DamageEvent -> [DamageEvent.DamageEvent]
splitExcess gs event =
  Maybe.fromMaybe [event] $ do
    oid <- Recipient.objectOf (DamageEvent.target event)
    bar <- excessThreshold gs (DamageEvent.source event) (DamageEvent.target event)
    let excess = Natural.minusSaturating (DamageEvent.amount event) bar
    Monad.guard (excess > 0)
    player <- Projection.controllerOf oid gs
    pure $
      [event {DamageEvent.amount = bar} | bar > 0]
        <> [damageEvent gs (DamageEvent.kind event) (DamageEvent.source event) (Recipient.ToPlayer player) excess]

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
-- CR 120.4b and CR 120.4c only. CR 120.4a runs BEFORE this, on the event list
-- rather than on the batch: redirectExcess above has already split any event
-- whose effect stated where its excess goes, so what arrives here is what CR
-- 120.4b deals. Pawl.DamageSpec's ExcessDamage group is the proof.
--
-- Two halves, so a caller that brackets the batch as one event can keep CR
-- 702.15e's separate life gain events OUT of the bracket: `processDamage` is the
-- batch, and `recordLifelinkGains` lands its gains after it. This is the
-- unbracketed composition, and the shape every caller but dealWave wants.
applyDamage :: [DamageEvent.DamageEvent] -> Game ()
applyDamage events = processDamage events >>= recordLifelinkGains

-- applyDamage minus lifelink's gain RECORDS, returning the survivors so the
-- caller can record those where its own bracket does not reach. The gain
-- itself -- the life total -- is written here, by `gainOne`, since CR 120.4c
-- processes it with the damage's other results (see the note on `lost`).
processDamage :: [DamageEvent.DamageEvent] -> Game [DamageEvent.DamageEvent]
processDamage events = do
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
        Recipient.ToPile _ -> g
        -- CR 120.3a's life loss is NOT here. It is the one result of CR 120.3 a
        -- replacement effect can rewrite (CR 120.4c, Worship), so it goes through
        -- Event.resolveLifeLoss and the CR 616.1 loop -- a Game action, where this
        -- fold is pure. The `lost` pass below performs it, exactly as
        -- `counterResults` below performs CR 120.3b/d/g's counters, and for the
        -- same reason.
        Recipient.ToPlayer _ -> g
      -- CR 120.3f: lifelink damage gains its source's controller that much life,
      -- IN ADDITION to the damage's other results. A pass of its own over the same
      -- survivors, deliberately not a branch inside markOne: "in addition" is
      -- then structural, and no arm of CR 120.3 above can be turned into
      -- "instead" by an edit here. Run below, ahead of the life-loss pass, for
      -- the reason stated there.
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
      -- A pass of its own rather than a line inside markOne, for `gainOne`'s reason:
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
      -- damage that caused it, and CR 603.3a's control sample would be taken
      -- before the cause it describes.
      --
      -- Only where life was actually lost. CR 120.3b's infect diversion replaces
      -- the life loss with poison counters, and a 0-damage event loses nothing --
      -- neither is a life loss event for CR 702.179d to see. Only players: CR
      -- 120.3c and CR 120.3d take a permanent's damage somewhere else entirely.
      --
      -- Infect and NOT wither, which is CR 120.3a's own wording. Pinned by
      -- DamageSpec's Wither group, which asserts the recorded LifeLost event and
      -- not merely the total.
      -- The amount is the one the `lost` pass below settled and wrote, never the
      -- damage: under Worship a player dealt 10 at 5 life loses 4, and the record
      -- has to say so or a "whenever you lose life" clause reads the wrong number.
      lifeLostBy ev losing = case DamageEvent.target ev of
        Recipient.ToPlayer pid
          | not (DamageEvent.dealtByInfect ev),
            losing > 0 ->
              [GameEvent.LifeLost (LifeChange.MkLifeChange pid losing)]
        _ -> []
      -- CR 120.3h's and CR 120.3c's counter removals, recorded so a trigger
      -- watching counters come off has an event to fire off: CR 310.12b's "when
      -- the last defense counter is removed" for the battle half, and Chandra,
      -- Fire Artisan's "whenever one or more loyalty counters are removed" for the
      -- planeswalker half. BOTH kinds, because the two rules differ in nothing but
      -- the counter kind -- the same reason `removeCounters` above is one function.
      --
      -- ONE record per PERMANENT per kind and not one per damage event, which is
      -- CR 510.2's simultaneity taken seriously: two unblocked attackers that
      -- between them take a defense-5 battle to 0 removed its last defense counter
      -- ONCE, so the Siege ability triggers once, and two that take two loyalty
      -- counters off a planeswalker fire Chandra's trigger once for two rather than
      -- twice for one. A per-event record would also have neither event reach 0 and
      -- the Siege trigger would never fire at all.
      --
      -- Read as a BEFORE/AFTER pair off the two boards rather than summed out of
      -- the events, so the record describes what actually came off: the floor in
      -- removeCounters above means 4 damage to a defense-1 battle removes one counter,
      -- not four. That is also why this is a DIFF rather than a call to
      -- Event.removeCounters, CR 122's funnel: `markOne` is a pure fold and the
      -- funnel is a Game action, so routing it would emit one event per damage
      -- event and lose both properties above.
      --
      -- Every PERMANENT a surviving event named, not only the ones tagged
      -- ToBattle or ToPlaneswalker: whether a counter actually came off is what the
      -- before/after pair below answers, so this need not classify the recipient a
      -- second time -- and a permanent that is a battle or a planeswalker under
      -- some other tag (CR 120.3h alongside CR 120.3e) is therefore not missed. A
      -- permanent that is both yields two records, one per kind, which is what CR
      -- 120.3's "one or more of the following results" says.
      permanentHit ev = Recipient.objectOf (DamageEvent.target ev)
      -- Battle.defenseOn generalized over the kind, which is the whole change: it
      -- is that function's body with CounterKind.Defense taken as an argument.
      countersOn kind oid gs = maybe 0 (Map.findWithDefault 0 kind . Object.counters) (Game.lookupObject oid gs)
      removalOn kind before after oid =
        let was = countersOn kind oid before
            now = countersOn kind oid after
         in [GameEvent.CountersRemoved (CounterChange.MkCounterChange oid kind was now) | was > now]
      -- The `was > now` guard above already subsumes the card-type classification,
      -- so this folds both kinds over every hit permanent rather than asking which
      -- of CR 120.3's results the recipient earned: a creature has neither kind on
      -- it and yields neither record.
      removalsBetween before after =
        concatMap
          (\oid -> concatMap (\kind -> removalOn kind before after oid) [CounterKind.Loyalty, CounterKind.Defense])
          (List.nub (Maybe.mapMaybe permanentHit survivors))
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
                      Recipient.ToPile _ -> pure ()
  -- CR 120.3f's gain, performed HERE and not in the fold below, because CR 120.4c
  -- processes the damage into ALL of its results as one event and the life-loss
  -- pass that follows reads the resulting total off the live board (see the note
  -- on `lost`). Writing the gain afterwards let a floor row (Worship) fire on a
  -- drain a simultaneous gain covered; see #2563.
  --
  -- Ahead of `markOne` and `tallyOne` too, which changes nothing they can see:
  -- those write GameState.objects and Player.commanderDamage, and this writes
  -- Player.life. The RECORD lands later still, in recordLifelinkGains, after
  -- the damage that caused it.
  State.modify' (\gs -> List.foldl' gainOne gs survivors)
  -- CR 120.4c: "damage that's been dealt is processed into its results, as
  -- modified by replacement effects that interact with those results (such as
  -- life loss or counters)". This is that step for CR 120.3a's life loss, and the
  -- counter half of the same sentence is `counterResults` below.
  --
  -- A monadic pass rather than an arm of `markOne`, because Event.resolveLifeLoss
  -- runs the CR 616.1 loop and that fold is pure -- `counterResults`' split
  -- exactly. It runs BEFORE the fold, which reorders the player write against the
  -- object marks and nothing else; the two touch disjoint maps, and nothing
  -- observes the board in between.
  --
  -- The guard is CR 120.3a's own wording, and it is `lifeLostBy`'s below: infect
  -- replaces the life loss with poison (CR 120.3b), which `counterResults` places.
  -- WITHER IS ABSENT ON PURPOSE -- CR 120.3d pairs it with infect for a CREATURE
  -- recipient only, so a wither source drains a player like any other. A 0-damage
  -- event needs no guard here: Event.resolveLifeLoss declines a zero itself.
  --
  -- Sequential in the running life total, and that is the rule rather than an
  -- accident: each proposal reads the live board, which the previous one has
  -- already written, so a player at 2 taking two simultaneous lethal hits under
  -- Worship has the first cut to a loss of 1 and the second cut to nothing, and
  -- ends at 1. Reading every proposal against the pre-batch board would cut both
  -- to 1 and kill them.
  --
  -- The same reading is why CR 120.3f's gain is written ABOVE rather than in the
  -- fold below: the board each proposal reads has to be the one the WHOLE damage
  -- event's results leave, gains included. CR 120.4d's second Example is that
  -- board -- at 2, with results "loses 5 life" and "gains 5 life", "Worship's
  -- effect sees that the damage event would not reduce the player's life total to
  -- less than 1, so Worship's effect is not applied" -- and Pawl.ReplacementSpec's
  -- Worship group proves it with a lifelink blocker in place of its Awe Strike.
  --
  -- The DAMAGE is untouched: `gainOne`, `tallyOne` and the DamageDealt record
  -- below all still read DamageEvent.amount. CR 120.4b dealt it, and this
  -- replaces a RESULT of it -- Worship's ruling, "any damage rendered useless by
  -- Worship was still dealt".
  --
  -- Aligned with `survivors` by position, so the fold below can zip the two.
  lost <-
    Monad.forM survivors $ \ev -> case DamageEvent.target ev of
      Recipient.ToPlayer pid | not (DamageEvent.dealtByInfect ev) -> do
        n <- Event.resolveLifeLoss LifeLossCause.ByDamage pid (DamageEvent.amount ev)
        Monad.when (n > 0) . State.modify' $ \gs ->
          let drain player = player {Player.life = Player.life player - toInteger n}
           in gs {GameState.players = Map.adjust drain pid (GameState.players gs)}
        pure n
      _ -> pure 0
  -- CR 608.2i: each surviving event is RECORDED, not enqueued. Sba consumes by
  -- bumping GameState.damageScannedThrough; the record survives the check.
  --
  -- CR 615.13's preventions are recorded FIRST, which is the order they happened
  -- in: a prevention is applied inside the CR 616.1 loop, which settles what the
  -- event will be BEFORE the surviving damage is dealt. Both records land inside
  -- one CR 117.5 boundary, so the two kinds of trigger are gathered together
  -- either way and CR 603.3b lets their controller order them; what this fixes is
  -- the canonical order the prompt indexes into.
  --
  -- One record per APPLICATION, never per recipient: groupPreventions has already
  -- collapsed the batch to CR 615.13's own unit, and the record carries the whole
  -- per-recipient map so that a trigger scoped to one of them can still read its
  -- share.
  --
  -- And only the applications that PREVENTED something are recorded, which is CR
  -- 615.13's own condition: such an ability triggers each time a prevention
  -- effect is applied to one or more simultaneous damage events "and prevents
  -- some or all of that damage". CR 615.12's inert application is exactly the one
  -- that prevents none of it, and it is on this list at an amount of 0 --
  -- Replacement.preventionBy puts it there so that its CR 615.5 rider still
  -- queues below, which is why the gate is here rather than on the list. The
  -- proof is Phyrexian Vindicator's trigger staying silent against Spider-Punk's
  -- unpreventable damage while Phantom Tiger's counter still comes off
  -- (Pawl.ReplacementSpec).
  State.modify'
    ( \gs ->
        let marked = List.foldl' markOne gs survivors
            tallied = List.foldl' tallyOne marked survivors
            noted = List.foldl' (\g p -> Event.recordEvent (GameEvent.DamagePrevented (DamagePrevented.MkDamagePrevented (Prevention.by p) (Prevention.source p) (Prevention.amounts p))) g) tallied (filter (\p -> sum (Prevention.amounts p) > 0) prevented)
            dealt = List.foldl' (\g ev -> Event.recordEvent (GameEvent.DamageDealt ev) g) noted survivors
         in -- CR 119.2's life loss is recorded AFTER the damage that caused it,
            -- which is the same reasoning the prevention/damage order above
            -- follows: both land inside one CR 117.5 boundary, so the triggers
            -- are gathered together either way, and this fixes only the
            -- canonical order. Simultaneous with the damage under CR 119.2, and
            -- logged after it because a cause reads before its consequence.
            --
            -- CR 120.3f's gain is NOT here: recordLifelinkGains lands it after
            -- the whole batch, outside any bracket the caller put around it,
            -- because CR 702.15e makes each source's gain an event of its own.
            --
            -- CR 120.3c's and CR 310.6's counter removals join the losses, after
            -- the damage and for the same reason: they are a RESULT of it (CR
            -- 120.3c, CR 120.3h), so the cause reads first. `marked` is the board
            -- markOne left, which is what makes the pair the removal that
            -- actually happened.
            List.foldl'
              (flip Event.recordEvent)
              dealt
              (concatMap (uncurry lifeLostBy) (zip survivors lost) <> removalsBetween board marked)
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
  --
  -- Filtered on the RIDER and not on the amount, which is the record above's
  -- gate exactly reversed: CR 615.12 says an inapplicable prevention is still
  -- applied and "any
  -- additional effects they have will take place", so an application that
  -- prevented 0 owes its rider just the same. An amount-scaled rider then reads
  -- 0 through Binding.eventAmount and does nothing of its own accord (Test of
  -- Faith puts on no counter), which is the rule as well.
  State.modify' $ \gs ->
    gs {GameState.pendingPreventionRiders = GameState.pendingPreventionRiders gs <> Seq.fromList (filter (Maybe.isJust . Prevention.rider) prevented)}
  pure survivors

-- CR 120.3f's gain, RECORDED -- processDamage's `gainOne` performs it -- so that
-- "whenever you gain life" sees lifelink (CR 702.15b) and not only an effect
-- that says the words. The record lands here and the gain itself earlier, which
-- is the split `lifeLostBy` there already has.
--
-- A function of its own, run AFTER processDamage and so after any bracket a
-- caller puts around it, because CR 702.15e makes each lifelink source's gain a
-- life gain EVENT of its own: "if multiple sources with lifelink deal damage at
-- the same time, they cause separate life gain events". dealWave brackets a
-- combat damage step as one Pawl.Types.EventGroup (CR 510.2), and a gain
-- recorded inside that bracket shares it, which fused two attackers' gains into
-- one trigger event for TriggerCondition.PlayersGainLife. At depth 0 -- every
-- caller today -- each record is a group of its own; a bracket an outer caller
-- left open would take them all, as it takes everything. Pawl.EventTriggerSpec's
-- lifelinkGainEventsSpec is the proof, and its damage-group assertion pins that
-- the damage stayed bracketed.
--
-- Logged after the damage, its loss and its counter results, which is the
-- order a cause reads before its consequence. All of it lands inside one CR
-- 117.5 boundary, so the triggers are gathered together either way and this
-- fixes only the canonical order.
--
-- ONE record per SOURCE, which is both directions of CR 702.15e at once: two
-- sources are two events, so summing them first would give a Pridemate one
-- counter where it is owed two; one source dealing damage to several recipients
-- at the same time is ONE event, so recording per damage event would give it two
-- where it is owed one. CR 119.9 reads the trigger as "whenever a source causes
-- [a player] to gain life", and the amount is that source's whole damage.
--
-- Keyed on the gainer as well as the source, which no board reaches today --
-- CR 702.15b resolves one source's lifelink to one player -- and is what the key
-- MEANS rather than a defence: the record names a player, so two players would
-- be two gains however few sources caused them.
--
-- First-appearance order, so the records read in the order the batch dealt its
-- damage. Pawl.EventTriggerSpec's lifelinkGainEventsSpec is the proof of both
-- directions.
--
-- Whom the damage was dealt to is none of this function's business, exactly as
-- it is none of `gainOne`'s: CR 702.15b hangs the gain off the SOURCE, so damage
-- to a creature or a planeswalker gains life just as damage to a player does.
-- `lifeLostBy` scopes to a player recipient because CR 120.3a does; this one
-- must not.
--
-- CR 119.9's zero guard, stated HERE rather than left to the callers, and the
-- same posture `lifeLostBy` takes. No producer in the pool builds a 0-amount
-- event today -- CR 510.1a makes `attackerAssignment` drop a creature assigning
-- 0 or less, and Resolve's DealDamage arm guards its own quantity -- so this
-- restates the rule at the site that writes the record instead of resting on
-- an invariant two modules away.
recordLifelinkGains :: [DamageEvent.DamageEvent] -> Game ()
recordLifelinkGains survivors =
  State.modify' $ \gs ->
    List.foldl'
      (\g (pid, total) -> Event.recordEvent (GameEvent.LifeGained (LifeChange.MkLifeChange pid total)) g)
      gs
      (lifelinkGains survivors)

-- One (source, gainer) pair's damage, summed, in the order the sources first
-- appear in the batch. The grouping recordLifelinkGains above records.
lifelinkGains :: [DamageEvent.DamageEvent] -> [(PlayerId, Natural)]
lifelinkGains survivors =
  let keyed =
        Maybe.mapMaybe
          ( \ev -> case DamageEvent.dealtByLifelink ev of
              Just pid | DamageEvent.amount ev > 0 -> Just ((DamageEvent.source ev, pid), DamageEvent.amount ev)
              _ -> Nothing
          )
          survivors
      totals = Map.fromListWith (+) keyed
   in fmap (\key -> (snd key, Map.findWithDefault 0 key totals)) (List.nub (fmap fst keyed))

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
--
-- ONE Pawl.Types.EventGroup for the whole wave's damage, which is CR 510.2 in
-- as many words: "all combat damage that's been assigned is dealt
-- simultaneously". CR 603.2c's batch conditions are what a board can read that
-- with -- TriggerCondition.PermanentsDealCombatDamageToPlayer fires once for the
-- step where its per-damager twin fires once per event, and Pawl.Engine.Event's
-- batchScoped is that fork. The life loss and counter removals processDamage
-- records ride inside the bracket too: CR 120.3's results of the damage,
-- simultaneous with it. Lifelink's gains do NOT: CR 702.15e makes each source's
-- gain a life gain event of its own, so recordLifelinkGains lands them once the
-- bracket has closed. Per WAVE and not per combat: CR 510.4's second combat
-- damage step is a second step, so a double striker that connects in both is
-- two occurrences.
dealWave :: (ObjectId -> Bool) -> Game ()
dealWave assigns = do
  assignment <- gatherCombatDamage assigns
  survivors <- Event.simultaneously (processDamage assignment)
  recordLifelinkGains survivors

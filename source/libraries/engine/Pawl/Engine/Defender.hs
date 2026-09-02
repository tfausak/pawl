-- Who is defending: CR 506.2's designation for the combat phase, and CR 508.5's
-- "the defending player" for an ability of an attacking creature. Its own module
-- rather than functions in Pawl.Engine.Combat because unrelated halves of the
-- engine ask them and several sit BELOW combat: Pawl.Engine.Target narrows a
-- target slot by CR 508.5 (CR 702.39a's "target creature defending player
-- controls") and Combat itself imports Pawl.Engine.Event, which reaches Target,
-- while Pawl.Engine.Projection and Pawl.Engine.CombatRestriction both read the
-- designation and Combat imports both -- so the questions have to live under all
-- of them.
module Pawl.Engine.Defender where

import qualified Control.Applicative as Applicative
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Combat as Combat
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)

-- CR 506.2 / CR 802.2: the players defending the combat in progress, in APNAP
-- order (CR 101.4) -- the order CR 802.4 has them declare blockers in and CR
-- 802.5 has them assign combat damage in. Empty outside combat, and on a combat
-- phase whose beginning-of-combat turn-based action found no opponent.
--
-- The one place the designation is read, so that CR 802's several defending
-- players arrive here rather than at each reader. Exactly Pawl.Types.Combat's
-- defenders field: the beginning of combat step settles the whole group, one
-- player under CR 507.1 and every opponent under CR 802.2, so nothing is left
-- for this function to derive.
--
-- A LIST and not one player, because CR 802.2a denies that several defending
-- players can be folded into one: a reader wanting "a defending player" wants
-- one specific one, resolved per attacking creature from what that creature is
-- attacking (playerOf below), never the whole group.
defendingPlayers :: GameState -> [PlayerId]
defendingPlayers = Combat.defenders . GameState.combat

-- CR 508.5 / CR 802.2a: the defending player an attacking creature's ability
-- refers to -- "the player that creature is attacking, the controller of the
-- planeswalker that creature is attacking, or the protector of the battle that
-- creature is attacking". One arm per AttackTarget arm, because that rule's case
-- split IS this type's, and it is the narrowing CR 802.2a demands: with several
-- defending players "a defending player" is resolved here, per attacking
-- creature, and never off the group.
--
-- CR 310.9d is the battle arm's other half, and it is wider than CR 508.5: while a
-- battle is being attacked, EVERY rule and effect that refers to the defending
-- player relative to it means the protector. Reading it off the protector here
-- rather than off the combat record is what makes that true of a battle whose
-- controller is the attacking player -- the case CR 310.9b's "notably" creates.
--
-- The controller lookup is a PARAMETER because this module sits below
-- Pawl.Engine.Projection -- Projection reads the designation from here -- and CR
-- 508.5's planeswalker arm is a layer-2 question. Every caller passes
-- Projection.controllerWithLastKnown, which is what CR 608.2h's last known
-- information asks for once CR 704.5i has buried the planeswalker: reading the
-- object itself would answer nobody, and reading its owner the wrong seat.
-- Pawl.CombatEffectSpec's LastKnownDefendingPlayer group is those two falsifiers.
--
-- CR 506.4 is why the lookup serves CR 802.2a's PRESENT tense on its own: a
-- planeswalker whose controller changes is removed from combat, so "controls" and
-- "controlled before it was removed" cannot disagree while the creature is still
-- attacking it. Once they can -- which is exactly once the removal has happened --
-- rule 802.2a's third sentence is the answer, and playerOfAttacker below is where
-- it is read, that seat being recorded per ATTACKER.
--
-- Nothing means the target names no player: no defending player at all (outside
-- combat), or a battle mid-repair with no designation (CR 310.11).
playerOf :: (ObjectId -> GameState -> Maybe PlayerId) -> AttackTarget.AttackTarget -> GameState -> Maybe PlayerId
playerOf controllerOf target gs = case target of
  AttackTarget.OfPlayer pid -> Just pid
  AttackTarget.OfPlaneswalker pw -> controllerOf pw gs
  -- CR 310.9d while the battle is there: the protector is the defending player
  -- relative to it, including CR 310.11's mid-repair battle whose designation
  -- names nobody. Once it has left, CR 506.4c keeps the creature attacking a
  -- battle with no live object, and CR 508.5's second sentence still asks for its
  -- protector -- so the second arm is CR 608.2h's filed designation
  -- (Pawl.Types.LastKnown's protector) and never the head of defendingPlayers,
  -- which CR 802.2a denies is an answer at all once several players defend.
  --
  -- Both arms answer a DECLARED attack only where no seat was recorded, which in
  -- a running game is never: defenderOfAttack below reads rule 508.5's second
  -- sentence off Pawl.Types.Combat's attackedUnder, and reaches this function
  -- only for a combat record built by hand. That leaves the filed designation
  -- with no observer (#2985).
  --
  -- Battlefield membership rather than a missing object, the question
  -- attackableBattles already asks: the two arms are "is it there" and "was it
  -- there", so a battle still in GameState.objects but in another zone reads as
  -- gone rather than carrying a stale designation forward.
  AttackTarget.OfBattle oid
    | Set.member oid (GameState.battlefield gs) -> Battle.protectorOf oid gs
    | otherwise -> Battle.lastKnownProtectorOf oid gs

-- The same rule, asked of the attacking CREATURE rather than of what it attacks
-- -- which is the shape every caller but the declaration's own event wants, rule
-- 508.5 being phrased about a creature. Nothing when the object is not attacking
-- at all, which is the honest answer: a creature that is not an attacker has no
-- defending player.
--
-- CR 802.2a's THIRD sentence lives here rather than in playerOf, because the seat
-- it names -- "the controller of the planeswalker that creature was attacking
-- before it was removed from combat" -- is recorded per attacker
-- (Pawl.Types.Combat's attackedUnder) and playerOf sees only the target. CR 506.4
-- is what makes the record answer both tenses at once: it removes the planeswalker
-- the moment its controller changes, so the recorded seat and the live one differ
-- for precisely the creature that is no longer attacking it.
--
-- BOTH permanent arms, and for one reason. A battle's defending player is its
-- protector (CR 310.9d), and that designation moves under CR 704.5y as well as
-- under CR 310.9f: a protector who takes the battle becomes a player who can't
-- protect it (CR 310.12a), so rule 704.5y re-chooses in the same breath that CR
-- 506.4 removes the battle from combat. A battle's two tenses come apart exactly
-- where a planeswalker's do, so both read the record. An attacked player is the
-- seat itself.
--
-- playerOf's own live arms answer for an attacker with no recorded seat: a combat
-- record built by hand rather than declared. In a running game both writers of
-- Combat.attackers record one.
playerOfAttacker :: (ObjectId -> GameState -> Maybe PlayerId) -> ObjectId -> GameState -> Maybe PlayerId
playerOfAttacker controllerOf attacker gs =
  (\target -> defenderOfAttack controllerOf attacker target gs)
    =<< Map.lookup attacker (Combat.attackers (GameState.combat gs))

-- playerOfAttacker with the target already in hand, for a caller walking
-- Combat.attackers' own entries (Pawl.Engine.Damage).
defenderOfAttack :: (ObjectId -> GameState -> Maybe PlayerId) -> ObjectId -> AttackTarget.AttackTarget -> GameState -> Maybe PlayerId
defenderOfAttack controllerOf attacker target gs = case target of
  AttackTarget.OfPlayer {} -> playerOf controllerOf target gs
  _ ->
    Map.lookup attacker (Combat.attackedUnder (GameState.combat gs))
      Applicative.<|> playerOf controllerOf target gs

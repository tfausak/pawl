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

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
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
-- CR 506.4 is why one lookup serves both of CR 802.2a's tenses: a planeswalker
-- whose controller changes is removed from combat, so "controls" and "controlled
-- before it was removed" cannot disagree while the creature is still attacking it.
--
-- Not implemented: CR 802.2a for a creature whose BATTLE has left the
-- battlefield. That arm answers the first defending player, which is exact only
-- while the list is a singleton; CR 310.9's protector is not a characteristic, so
-- Pawl.Types.LastKnown does not file one and there is nothing to read (#2844).
--
-- Nothing means the target names no player: no defending player at all (outside
-- combat), or a battle mid-repair with no designation (CR 310.11).
playerOf :: (ObjectId -> GameState -> Maybe PlayerId) -> AttackTarget.AttackTarget -> GameState -> Maybe PlayerId
playerOf controllerOf target gs = case target of
  AttackTarget.OfPlayer pid -> Just pid
  AttackTarget.OfPlaneswalker pw -> controllerOf pw gs
  -- CR 310.9d while the battle is there: the protector is the defending player
  -- relative to it, including CR 310.11's mid-repair battle whose designation
  -- names nobody. Once it has left, CR 506.4c keeps the creature attacking with
  -- no battle to read. Battlefield membership rather than a missing object, the
  -- question attackableBattles already asks, so it does not rest on the departed
  -- id having been purged from GameState.objects.
  --
  -- The GUARD itself is a regression fence rather than a proven behavior. An
  -- attacked battle's protector and defendingPlayers cannot differ today:
  -- attackableBattles admits a battle only when they agree, CR 506.4 removes a
  -- battle from combat the moment its protector changes, and CR 704.5x's rider
  -- suspends the repair while it is attacked -- so on every board pawl can build
  -- the two arms answer the same seat, and no test separates them. The elision
  -- that keeps it that way is Combat.stillAttackedBattle's, see #853.
  AttackTarget.OfBattle oid
    | Set.member oid (GameState.battlefield gs) -> Battle.protectorOf oid gs
    | otherwise -> Maybe.listToMaybe (defendingPlayers gs)

-- The same rule, asked of the attacking CREATURE rather than of what it attacks
-- -- which is the shape every caller outside Pawl.Engine.Damage wants, rule 508.5
-- being phrased about a creature. Nothing when the object is not attacking at
-- all, which is the honest answer: a creature that is not an attacker has no
-- defending player.
playerOfAttacker :: (ObjectId -> GameState -> Maybe PlayerId) -> ObjectId -> GameState -> Maybe PlayerId
playerOfAttacker controllerOf attacker gs =
  (\target -> playerOf controllerOf target gs)
    =<< Map.lookup attacker (Combat.attackers (GameState.combat gs))

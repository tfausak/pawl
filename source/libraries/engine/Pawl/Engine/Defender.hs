-- CR 508.5: who "the defending player" is, for an ability of an attacking
-- creature. Its own module rather than a function in Pawl.Engine.Combat because
-- two unrelated halves of the engine ask it and one of them sits BELOW combat:
-- Pawl.Engine.Target narrows a target slot by it (CR 702.39a's "target creature
-- defending player controls"), and Combat itself imports Pawl.Engine.Event,
-- which reaches Target -- so the question has to live under both.
module Pawl.Engine.Defender where

import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Combat as Combat
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)

-- CR 508.5: the defending player an attacking creature's ability refers to --
-- "the player that creature is attacking, the controller of the planeswalker that
-- creature is attacking, or the protector of the battle that creature is
-- attacking". One arm per AttackTarget arm, because that rule's case split IS this
-- type's.
--
-- CR 310.9d is the battle arm's other half, and it is wider than CR 508.5: while a
-- battle is being attacked, EVERY rule and effect that refers to the defending
-- player relative to it means the protector. Reading it off the protector here
-- rather than off Combat.defender is what makes that true of a battle whose
-- controller is the attacking player -- the case CR 310.9b's "notably" creates.
--
-- The planeswalker arm answers CR 508.5's BOTH sentences with the combat record's
-- defending player, and never asks the planeswalker. Both ways a target is
-- recorded draw it from Pawl.Engine.Combat.attackTargets -- CR 508.1b's
-- declaration and CR 508.4's entry -- and that list offers only planeswalkers the
-- defending player controls, so while the planeswalker is attacked its controller
-- IS that player. Once CR 506.4 removes it from combat the rule's second sentence
-- wants the controller it had "before it was removed from combat", which is the
-- same seat; reading the object instead answers NOBODY once CR 704.5i has buried
-- it, since the burial leaves no object to read a controller off, and answers the
-- owner on any board where the two seats differ. Pawl.CombatSpec's
-- LastKnownDefendingPlayer group is that board, built with a Confiscate. CR 702.19e
-- is what settles that a creature attacking a removed planeswalker still HAS a
-- defending player: it assigns damage to one.
--
-- CR 802's attack-multiple-players option is what would break the identity, since
-- several defending players make the record's one player the wrong answer for some
-- attacker; pawl has no options concept to read it from (#175), and this arm reads
-- the per-attacker record again when it arrives.
--
-- Nothing means the target names no player: no defending player at all (outside
-- combat), a battle that has left the battlefield, or a battle mid-repair with no
-- designation (CR 310.11).
--
-- The BATTLE arm still reads live, so CR 508.5's second sentence is unanswered for
-- a battle removed from combat -- its protector goes with it and this answers
-- Nothing (#1248).
playerOf :: AttackTarget.AttackTarget -> GameState -> Maybe PlayerId
playerOf target gs = case target of
  AttackTarget.OfPlayer pid -> Just pid
  AttackTarget.OfPlaneswalker _ -> Combat.defender (GameState.combat gs)
  AttackTarget.OfBattle oid -> Battle.protectorOf oid gs

-- The same rule, asked of the attacking CREATURE rather than of what it attacks
-- -- which is the shape every caller outside Pawl.Engine.Damage wants, rule 508.5
-- being phrased about a creature. Nothing when the object is not attacking at
-- all, which is the honest answer: a creature that is not an attacker has no
-- defending player.
playerOfAttacker :: ObjectId -> GameState -> Maybe PlayerId
playerOfAttacker attacker gs =
  (\target -> playerOf target gs)
    =<< Map.lookup attacker (Combat.attackers (GameState.combat gs))

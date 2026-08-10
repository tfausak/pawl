-- CR 508.5: who "the defending player" is, for an ability of an attacking
-- creature. Its own module rather than a function in Pawl.Engine.Combat because
-- two unrelated halves of the engine ask it and one of them sits BELOW combat:
-- Pawl.Engine.Target narrows a target slot by it (CR 702.39a's "target creature
-- defending player controls"), and Combat itself imports Pawl.Engine.Event,
-- which reaches Target -- so the question has to live under both.
module Pawl.Engine.Defender where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.Projection as Projection
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
-- CR 310.8d is the battle arm's other half, and it is wider than CR 508.5: while a
-- battle is being attacked, EVERY rule and effect that refers to the defending
-- player relative to it means the protector. Reading it off the protector here
-- rather than off Combat.defender is what makes that true of a battle whose
-- controller is the attacking player -- the case CR 310.8b's "notably" creates.
--
-- Nothing means the target names no player: a planeswalker or battle that has left
-- the battlefield, or a battle mid-repair with no designation (CR 310.10).
--
-- CR 508.5's second sentence -- the defending player of a creature that is no
-- longer attacking, read off what it WAS attacking before it left combat -- is last
-- known information, and this reads live instead. Unreachable in the pool, which
-- has no card that can remove an attacked permanent from combat and change who
-- defends through it (#537), and unobservable besides.
playerOf :: [Projection.ControlGrant] -> AttackTarget.AttackTarget -> GameState -> Maybe PlayerId
playerOf grants target gs = case target of
  AttackTarget.OfPlayer pid -> Just pid
  AttackTarget.OfPlaneswalker oid -> Projection.controllerOfGiven grants Set.empty oid gs
  AttackTarget.OfBattle oid -> Battle.protectorOf oid gs

-- The same rule, asked of the attacking CREATURE rather than of what it attacks
-- -- which is the shape every caller outside Pawl.Engine.Damage wants, rule 508.5
-- being phrased about a creature. Nothing when the object is not attacking at
-- all, which is the honest answer: a creature that is not an attacker has no
-- defending player.
playerOfAttacker :: [Projection.ControlGrant] -> ObjectId -> GameState -> Maybe PlayerId
playerOfAttacker grants attacker gs =
  (\target -> playerOf grants target gs)
    =<< Map.lookup attacker (Combat.attackers (GameState.combat gs))

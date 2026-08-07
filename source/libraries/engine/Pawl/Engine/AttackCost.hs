-- CR 508.1c / 508.1h / 613.11: the continuous effects that make ATTACKING COST
-- something. One of the modules on the axis CR 613.11 reaches past the layer
-- system (alongside Pawl.Engine.PlayerEffect, Pawl.Engine.BlockRequirement,
-- Pawl.Engine.AttackRequirement and Pawl.Engine.CombatRestriction). None is a
-- layer, and Pawl.Engine.Projection sees none of them.
--
-- This module's question is wider than its siblings' by its second argument,
-- which comes from the card read through CR 508.1b's announcement -- Ghostly
-- Prison says "attacking YOU" -- rather than from any rule about costs.
--
-- The only reader of Pawl.Types.AttackCost. Pawl.Engine.Combat asks for an
-- AMOUNT OF MANA and never learns which card produced one.
module Pawl.Engine.AttackCost where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ManaCost as ManaCost
import Pawl.Types.ObjectId (ObjectId)

-- CR 508.1h read for ONE announced attack: what must this creature's controller
-- pay for it to attack THAT target? One entry per printed cost in force,
-- unpooled, because CR 508.1h is what totals them.
--
-- The TARGET is an argument rather than derived from the combat record, because
-- CR 508.1b's announcement is step (b) and the total cost is step (h). Combat
-- asks this of a declaration not yet written to the record, and again of a
-- target no creature has been assigned to (attacksFreely below).
--
-- Ghostly Prison's "you" is the source's controller (CR 109.5), asked of the
-- TARGET rather than of the defending player: its own ruling is that a creature
-- that can't attack you can still attack a planeswalker you control, so an
-- AttackTarget.OfPlaneswalker is untaxed even when the planeswalker's
-- controller holds the Prison. The direction matters as much as the exemption:
-- a Prison its own controller is attacking WITH taxes nothing.
--
-- AttackTarget.OfBattle is untaxed for the same reason and one more: attacking a
-- battle someone protects is not attacking that player at all (CR 310.8b), and
-- the battle is not a player, so the OfPlayer comparison below cannot match it
-- however the board is arranged.
--
-- Empty for the board almost every game is played on: the battlefield walk
-- stops at `Face.attackCosts face` for every permanent that prints none, so no
-- projection is forced (#200).
costsOn :: ObjectId -> AttackTarget.AttackTarget -> GameState -> [ManaCost.ManaCost]
costsOn attacker target gs =
  let -- Hoisted out of the walk as CombatRestriction.restricted hoists them, and
      -- both unforced until some permanent actually declares a cost.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- CR 613.11 puts these effects after every layer, so the subject set is
      -- read against the FULL projection rather than a partial one.
      view = Projection.project attacker gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> []
        Just face -> case Face.attackCosts face of
          -- Every permanent in almost every game.
          [] -> []
          costs ->
            -- The same two ability losses CombatRestriction.restricted asks
            -- about: CR 305.7's basic-land subtype set, and CR 604.2 against a
            -- CR 613.1f layer-6 removal.
            if (null setEffs || Projection.liveGiven setEffs Set.empty source gs)
              && not (removed source)
              then concatMap (fromCost source) costs
              else []
      fromCost source ac =
        if Just target == fmap AttackTarget.OfPlayer (Projection.controllerOf source gs)
          && Projection.affects source attacker (AttackCost.subject ac) view gs
          then [AttackCost.perAttacker ac]
          else []
   in concatMap fromPermanent (Set.toList (GameState.battlefield gs))

-- CR 508.1h: every printed cost in force on every announced attack, pooled into
-- one mana cost. Ghostly Prison's "{2} for each creature" is performed rather
-- than restated -- the multiplication is the fold, so three taxed attackers owe
-- {2}{2}{2} and Pawl.Engine.Mana.spend sums the generic symbols.
--
-- CR 508.1h then locks the total in, which is the CALLER's to honour and cannot
-- be honoured here: this is a pure function of a game state.
-- Combat.declareAttackers calls it once, binds the result, and pays THAT.
totalCost :: Map ObjectId AttackTarget.AttackTarget -> GameState -> ManaCost.ManaCost
totalCost declaration gs =
  ManaCost.MkManaCost
    (concatMap ManaCost.unwrap (concatMap (\(oid, target) -> costsOn oid target gs) (Map.toList declaration)))

-- CR 508.1d's cost clause: a player is never required to pay a cost to attack.
-- True when SOME attack this creature could announce costs nothing, which is
-- when a requirement to attack still binds; false when every one of them costs.
--
-- ANY and not ALL, by Ghostly Prison's planeswalker ruling again: a creature
-- that could attack a planeswalker for free can attack without paying a cost,
-- so its requirement stands and the player's own CR 508.1b announcement decides
-- whether they end up paying.
attacksFreely :: ObjectId -> [AttackTarget.AttackTarget] -> GameState -> Bool
attacksFreely attacker targets gs = any (\target -> null (costsOn attacker target gs)) targets

-- CR 508.1c / 508.1h / 613.11: the continuous effects that make ATTACKING COST
-- something. One of the modules on the axis CR 613.11 reaches past the layer
-- system (alongside Pawl.Engine.PlayerEffect, Pawl.Engine.BlockRequirement,
-- Pawl.Engine.AttackRequirement and Pawl.Engine.CombatRestriction). None is a
-- layer, and Pawl.Engine.Projection sees none of them.
--
-- This module's question is wider than its siblings' by its second argument,
-- which comes from the card read through CR 508.1b's announcement -- Ghostly
-- Prison says "attacking YOU", Sphere of Safety "attacking you or planeswalkers
-- you control" -- rather than from any rule about costs.
--
-- The only reader of Pawl.Types.AttackCost. Pawl.Engine.Combat asks for an
-- AMOUNT OF MANA and never learns which card produced one.
module Pawl.Engine.AttackCost where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackCostScope as AttackCostScope
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PerCreature as PerCreature

-- CR 508.1h read for ONE announced attack: what must this creature's controller
-- pay for it to attack THAT target? One entry per printed cost in force,
-- unpooled, because CR 508.1h is what totals them.
--
-- The TARGET is an argument rather than derived from the combat record, because
-- CR 508.1b's announcement is step (b) and the total cost is step (h). Combat
-- asks this of a declaration not yet written to the record, and again of a
-- target no creature has been assigned to -- Combat.attackCeilingGiven applies CR
-- 508.1d's cost clause per (creature, target) pair, so it asks about
-- announcements nobody has made.
--
-- The "you" -- where the printing has one -- is the source's controller (CR
-- 109.5), asked of the TARGET rather than of the defending player. WHICH announcements that covers is the card's
-- own Pawl.Types.AttackCostScope: Ghostly Prison's ruling is that a creature
-- that can't attack you can still attack a planeswalker you control, so under
-- the narrow arm an AttackTarget.OfPlaneswalker is untaxed even when the
-- planeswalker's controller holds the Prison, while Sphere of Safety's "or
-- planeswalkers you control" (CR 306.6) taxes exactly that attack. The direction
-- matters as much as the exemption under both arms: a Prison its own controller
-- is attacking WITH taxes nothing.
--
-- AttackTarget.OfBattle is untaxed under both PROTECTING arms, and that is a rule
-- rather than an omission: attacking a battle someone protects is not attacking
-- that player at all (CR 310.9b). Under EveryAttack it is taxed like any other
-- announcement, Oppressive Rays' sentence naming no player to be protected.
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
            -- CR 613.1f layer-6 removal. CR 613.11 / 601.2f put this after every
            -- layer, which is why the CR 305.7 gate is liveAfterLayers rather
            -- than liveGiven -- the same reason `view` above is the full
            -- projection.
            if (null setEffs || Projection.liveAfterLayers setEffs source gs)
              && not (removed source)
              then concatMap (fromCost source) costs
              else []
      -- CR 508.1b's announcement judged against the source's controller. The
      -- wide arm asks the same question of the planeswalker's controller, which
      -- is CR 306.6's separate thing to attack rather than a second player: the
      -- guard is deliberately `Just c ==` on a controller that must EXIST, since
      -- two Nothings would otherwise tax an attack on a planeswalker nobody
      -- controls.
      --
      -- EveryAttack is the arm that reads NEITHER the target nor the controller:
      -- Oppressive Rays taxes the creature and not a player, so it covers an
      -- attack on a battle (CR 310.9b) as readily as one on its own controller.
      -- Ordered ahead of the controller lookup for that reason -- a taxing
      -- permanent nobody controls would otherwise stop taxing.
      protects source ac = case AttackCost.scope ac of
        AttackCostScope.EveryAttack -> True
        AttackCostScope.Controller -> case Projection.controllerOf source gs of
          Nothing -> False
          Just owner -> target == AttackTarget.OfPlayer owner
        AttackCostScope.ControllerAndPlaneswalkers -> case Projection.controllerOf source gs of
          Nothing -> False
          Just owner -> case target of
            AttackTarget.OfPlayer pid -> pid == owner
            AttackTarget.OfPlaneswalker pw -> Projection.controllerOf pw gs == Just owner
            AttackTarget.OfBattle _ -> False
      -- CR 508.1h reads the share LIVE, here, from the board the caller handed
      -- this function; the lock-in belongs to Pawl.Engine.Combat.declareAttackers,
      -- which binds totalCost's answer once. CR 109.5's "you" for the count is
      -- the taxing permanent's controller, the same player the guard above
      -- reads, and the source is the permanent itself so that a Filter.IsSource
      -- written inside the count names it.
      --
      -- An unanswerable count contributes no symbols rather than blocking the
      -- attack -- the honest reading, since a Quantity that cannot say how many
      -- has not said "many". No printed counted share can reach it: every one
      -- counts permanents in a zone, which always answers.
      shareOf source ac = case AttackCost.perAttacker ac of
        PerCreature.Fixed cost -> [cost]
        PerCreature.Counted quantity ->
          let context = Filter.contextFor (Projection.controllerOf source gs) (Just source)
              generic n = ManaCost.MkManaCost [ManaSymbol.Generic (Integer.toNaturalSaturating n)]
           in Maybe.maybeToList (fmap generic (Quantity.evaluate (Projection.fullView gs) context gs source quantity))
      fromCost source ac =
        if protects source ac
          && Projection.affects source attacker (AttackCost.subject ac) view gs
          then shareOf source ac
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

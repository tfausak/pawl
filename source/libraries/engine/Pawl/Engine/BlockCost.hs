-- CR 509.1b / 509.1d / 613.11: the continuous effects that make BLOCKING COST
-- something. One of the modules on the axis CR 613.11 reaches past the layer
-- system (alongside Pawl.Engine.PlayerEffect, Pawl.Engine.BlockRequirement,
-- Pawl.Engine.AttackRequirement, Pawl.Engine.CombatRestriction and
-- Pawl.Engine.AttackCost). None is a layer, and Pawl.Engine.Projection sees none
-- of them.
--
-- Pawl.Engine.AttackCost's twin, NARROWER by that module's second argument: an
-- attack is announced against something (CR 508.1b) and a cost to attack may be
-- judged against it, while CR 509.1d totals a cost to block over the chosen
-- CREATURES. So this module's question is about the blocker alone, and what it
-- is blocking never reaches it.
--
-- The only reader of Pawl.Types.BlockCost. Pawl.Engine.Combat asks for COSTS and
-- never learns which card wrote one -- each cost is tagged with the PERMANENT it
-- is printed on (costsOn below), which is an id and not an identity: Combat pays
-- what the tag names and never reads its text.
module Pawl.Engine.BlockCost where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Types.BlockCost as BlockCost
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PerCreature as PerCreature

-- CR 509.1d read for ONE creature: what must its controller pay for it to block?
-- One entry per printed cost in force, unpooled, because CR 509.1d is what totals
-- them.
--
-- No target argument, where AttackCost.costsOn takes one: see the module header.
-- A creature blocking two attackers under a Palace Guard therefore owes its share
-- ONCE, which is CR 509.1d totalling over the chosen creatures.
--
-- Each share is TAGGED with the permanent that printed it, AttackCost.costsOn's
-- pairing and its reason: CR 509.1d adds several printed costs into one total, so
-- the total is on no object, while each PART is on the permanent whose text
-- states it.
--
-- Empty for the board almost every game is played on: the battlefield walk stops
-- at `Face.blockCosts face` for every permanent that prints none, so no projection
-- is forced (#200).
costsOn :: ObjectId -> GameState -> [(ObjectId, Cost.Cost Keyword.Keyword)]
costsOn blocker gs =
  let -- Hoisted out of the walk as AttackCost.costsOn hoists them, and both
      -- unforced until some permanent actually declares a cost.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- CR 613.11 puts these effects after every layer, so the subject set is
      -- read against the FULL projection rather than a partial one.
      view = Projection.project blocker gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> []
        Just face -> case Face.blockCosts face of
          -- Every permanent in almost every game.
          [] -> []
          costs ->
            -- The same two ability losses AttackCost.costsOn asks about: CR
            -- 305.7's basic-land subtype set, and CR 604.2 against a CR 613.1f
            -- layer-6 removal.
            --
            -- Proved by Pawl.CombatEffectSpec's LandSubtypeStrip case "an
            -- animated Hollow Warrior set to Mountain costs nothing to block",
            -- which is where the group's per-reader case for this copy of the
            -- guard lives.
            if (null setEffs || Projection.liveAfterLayers setEffs source gs)
              && not (removed source)
              then concatMap (fromCost source) costs
              else []
      -- CR 509.1d reads the share LIVE, here, from the board the caller handed
      -- this function; the lock-in belongs to Pawl.Engine.Combat.declareBlockers,
      -- which binds totalCost's answer once. CR 109.5's "you" for the count is the
      -- taxing permanent's controller, and the source is the permanent itself so
      -- that a Filter.IsSource written inside the count names it.
      --
      -- An unanswerable count contributes no symbols rather than blocking the
      -- block -- the honest reading, since a Quantity that cannot say how many has
      -- not said "many".
      shareOf source bc = case BlockCost.perBlocker bc of
        PerCreature.Fixed cost -> [cost]
        PerCreature.Counted quantity ->
          let context = Filter.contextFor (Projection.controllerOf source gs) (Just source)
              generic n = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic (Integer.toNaturalSaturating n)])) []
           in Maybe.maybeToList (fmap generic (Quantity.evaluate (Projection.fullView gs) context gs source quantity))
      fromCost source bc =
        if Projection.affects source blocker (BlockCost.subject bc) view gs
          then fmap ((,) source) (shareOf source bc)
          else []
   in concatMap fromPermanent (Set.toList (GameState.battlefield gs))

-- CR 509.1d: every printed cost in force on every creature this declaration
-- chooses as a blocker, determined together. Charged once per CREATURE and not
-- once per pair, which is the rule's own unit -- a blocker assigned to two
-- attackers pays its share once.
--
-- A LIST and not one added-up cost, AttackCost.totalCost's shape and its reason:
-- the adding is Pawl.Engine.Cost.payToll's, which pools the mana so CR 509.1e
-- opens ONE window and pays each non-mana half against its own tag.
--
-- An entry naming NO attacker is not a chosen creature: CR 509.1a chooses a
-- creature by choosing something for it to block, and Pawl.Engine.Combat's
-- declaration map can carry an empty set.
--
-- CR 509.1d then locks the total in, which is the CALLER's to honour and cannot be
-- honoured here: this is a pure function of a game state.
-- Combat.declareBlockers calls it once, binds the result, and pays THAT.
totalCost :: Map ObjectId (Set.Set ObjectId) -> GameState -> [(ObjectId, Cost.Cost Keyword.Keyword)]
totalCost declaration gs =
  concatMap (\blocker -> costsOn blocker gs) (Map.keys (Map.filter (not . Set.null) declaration))

-- CR 509.1c's cost clause: a player is never required to pay a cost to block.
-- True when this creature can be declared as a blocker for nothing.
--
-- No pair to range over, where the attacking side asks costsOn of each
-- (creature, target) announcement CR 508.1b admits: the cost this asks about is
-- not judged against what is blocked, so a blocker is free or it is not.
blocksFreely :: ObjectId -> GameState -> Bool
blocksFreely blocker gs = null (costsOn blocker gs)

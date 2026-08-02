-- CR 508.1d / 508.1h / 613.11: the continuous effects that make ATTACKING COST
-- something. The fifth module on the axis CR 613.11 reaches past the layer system
-- -- Pawl.Engine.PlayerEffect answers rules questions about a PLAYER,
-- Pawl.Engine.BlockRequirement the one CR 509.1c asks about a pair of CREATURES,
-- Pawl.Engine.AttackRequirement the one CR 508.1d asks about a single one,
-- Pawl.Engine.CombatRestriction the two CR 508.1c and CR 509.1b ask about a single
-- one, and this the one CR 508.1d's cost clause asks about a creature AND what it
-- is attacking. None is a layer, and Pawl.Engine.Projection sees none of them --
-- CR 613.11 applies all five "after all other continuous effects have been
-- applied".
--
-- This module is the only reader of Pawl.Types.AttackCost. Pawl.Engine.Combat asks
-- for an AMOUNT OF MANA -- what does this declaration owe, and could this creature
-- have attacked for nothing -- and never learns which card produced one, the same
-- posture it takes toward Pawl.Engine.CombatRestriction's set of ids.
module Pawl.Engine.AttackCost where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Card as Card
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ManaCost as ManaCost
import Pawl.Types.ObjectId (ObjectId)

-- CR 508.1d's cost clause read for ONE announced attack: what must this
-- creature's controller pay for it to attack THAT target? One entry per printed
-- cost in force, unpooled, because CR 508.1h totals them and this is what it
-- totals.
--
-- The TARGET is an argument and not derived from the combat record, and that is
-- CR 508.1b's order arriving as a signature: "the active player announces which
-- player, planeswalker, or battle each of the chosen creatures is attacking" is
-- step (b), and the total cost is step (h), so what a creature is attacking is
-- settled before this can be asked. Pawl.Engine.Combat asks it of a declaration
-- that has not been written to the record yet, and again of a target no creature
-- has been assigned to at all (attacksFreely below).
--
-- Ghostly Prison's "YOU" is the source's controller (CR 109.5), and it is asked of
-- the TARGET rather than of the defending player: Ghostly Prison's own ruling is
-- that "a creature that can't attack you can still attack a planeswalker you
-- control", so an AttackTarget.OfPlaneswalker is untaxed even when the
-- planeswalker's controller is the one with the Prison. Reading the defending
-- player instead would tax that attack, and would also be indistinguishable from
-- reading the target on every board with no planeswalker on it -- which is every
-- board in the pool that has a Prison.
--
-- The direction matters as much as the exemption: a Prison its own controller is
-- attacking WITH taxes nothing, because nothing is attacking them.
--
-- Empty for the board almost every game is played on: the battlefield walk stops
-- at `Card.attackCosts card` for every permanent that prints none, so no
-- projection is forced and no control grant is walked (#200's posture).
costsOn :: ObjectId -> AttackTarget.AttackTarget -> GameState -> [ManaCost.ManaCost]
costsOn attacker target gs =
  let -- Hoisted out of the walk exactly as Pawl.Engine.CombatRestriction.restricted
      -- hoists them, and both unforced until some permanent actually declares a
      -- cost -- so a board carrying none pays for no projection.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- CR 613.11 puts these effects after every layer, so the subject set is read
      -- against the FULL projection rather than a partial one. Bound once for the
      -- one candidate this is asked about, and left unforced until a permanent
      -- actually declares a cost.
      view = Projection.project attacker gs
      fromPermanent source = case Game.cardOf source gs of
        Nothing -> []
        Just card -> case Card.attackCosts card of
          -- Every permanent in almost every game.
          [] -> []
          costs ->
            -- The same TWO ability losses Pawl.Engine.CombatRestriction.restricted
            -- asks about, for the same reasons and with the same unconditional cut:
            -- CR 305.7's basic-land subtype set, and CR 604.2's "remains on the
            -- battlefield and HAS THE ABILITY" against a CR 613.1f layer-6 removal.
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

-- CR 508.1h: "If any of the chosen creatures require paying costs to attack ...
-- the active player determines the total cost to attack." Every printed cost in
-- force on every announced attack, pooled into one mana cost -- which is Ghostly
-- Prison's "{2} FOR EACH creature they control that's attacking you" performed
-- rather than restated: the multiplication is the fold, so three taxed attackers
-- owe {2}{2}{2}, and Pawl.Engine.Mana.spend sums the generic symbols.
--
-- "Once the total cost is determined, it becomes 'locked in'. If effects would
-- change the total cost after this time, ignore this change." That is the
-- CALLER's to honour and cannot be honoured here: this is a pure function of a
-- game state, so it answers for the state it is given. Pawl.Engine.Combat's
-- declareAttackers calls it once, binds the result, and pays THAT -- never
-- recomputing between the determination and the payment.
totalCost :: Map ObjectId AttackTarget.AttackTarget -> GameState -> ManaCost.ManaCost
totalCost declaration gs =
  ManaCost.MkManaCost
    (concatMap ManaCost.unwrap (concatMap (\(oid, target) -> costsOn oid target gs) (Map.toList declaration)))

-- CR 508.1d's cost clause asked the way that rule asks it: "if a creature can't
-- attack unless a player pays a cost, that player is not required to pay that
-- cost, even if attacking with that creature would increase the number of
-- requirements being obeyed". True when SOME attack this creature could announce
-- costs nothing, which is when a requirement to attack still binds -- and false
-- when every one of them costs, which is when the rule lets the active player
-- walk away from the requirement.
--
-- ANY and not ALL, and the difference is Ghostly Prison's planeswalker ruling
-- again: a creature that could attack a planeswalker for free is one that can
-- attack without paying a cost, so its requirement stands and the player's own
-- CR 508.1b announcement decides whether they end up paying. With no planeswalker
-- on the defending player's side -- every board in the pool with a Prison on it --
-- the only candidate is the defending player, and the answer collapses to "is the
-- Prison's controller the one being attacked".
attacksFreely :: ObjectId -> [AttackTarget.AttackTarget] -> GameState -> Bool
attacksFreely attacker targets gs = any (\target -> null (costsOn attacker target gs)) targets

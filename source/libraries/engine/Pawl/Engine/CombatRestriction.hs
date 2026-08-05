-- CR 508.1c / 509.1b / 613.11: the continuous effects that FORBID an attack or
-- a block. One of the modules on the axis CR 613.11 reaches past the layer
-- system (alongside Pawl.Engine.PlayerEffect, Pawl.Engine.BlockRequirement and
-- Pawl.Engine.AttackRequirement). None is a layer, and Pawl.Engine.Projection
-- sees none of them.
--
-- The only reader of Pawl.Types.CombatRestriction, and the only module that may
-- case on it. Pawl.Engine.Combat asks for a SET OF IDS and never learns which
-- card produced one.
module Pawl.Engine.CombatRestriction where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)

-- CR 508.1c: which of `candidates` an effect in force right now says CAN'T
-- ATTACK. Pacifism's first half, and Blind-Spot Giant's when its gate is shut.
cantAttack :: [ObjectId] -> GameState -> Set ObjectId
cantAttack = restricted attacking

-- CR 509.1b: which of `candidates` an effect in force right now says CAN'T
-- BLOCK. Pacifism's second half, and Blind-Spot Giant's when its gate is shut.
cantBlock :: [ObjectId] -> GameState -> Set ObjectId
cantBlock = restricted blocking

-- CR 508.1c together with CR 506.5: which of `candidates` an effect in force
-- right now says can't be the ONLY creature declared as an attacker. Bonded
-- Construct.
--
-- The two above are answered ABOUT A CANDIDATE and this one is not, even though
-- all three come back as a set of ids: this set is the input to a whole
-- declaration's check (Pawl.Engine.Combat.attackDeclarationAllowed) rather than a
-- filter on the candidate list. See `restricted` below.
cantAttackAlone :: [ObjectId] -> GameState -> Set ObjectId
cantAttackAlone = restricted attackingAlone

-- The selectors, written out rather than a wildcard: an exhaustive case is what
-- makes a new arm a compile error at every site that would have to decide about
-- it.
attacking :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
attacking cr = case cr of
  CombatRestriction.CantAttack a _ -> Just a
  CombatRestriction.CantBlock _ _ -> Nothing
  CombatRestriction.CantAttackAlone _ _ -> Nothing

blocking :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
blocking cr = case cr of
  CombatRestriction.CantAttack _ _ -> Nothing
  CombatRestriction.CantBlock a _ -> Just a
  CombatRestriction.CantAttackAlone _ _ -> Nothing

attackingAlone :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
attackingAlone cr = case cr of
  CombatRestriction.CantAttack _ _ -> Nothing
  CombatRestriction.CantBlock _ _ -> Nothing
  CombatRestriction.CantAttackAlone a _ -> Just a

-- CR 508.1c / CR 509.1b's second clause: the condition the creature can't
-- attack (or block) UNLESS. Read off any arm, because the clause is the same
-- sentence in both rules: which declaration a restriction forbids, in what
-- shape, and whether it is gated are all independent, and Blind-Spot Giant
-- prints one gate across two arms.
--
-- Nothing is the UNCONDITIONAL restriction (Pacifism), not a gate that fails.
gate :: CombatRestriction.CombatRestriction -> Maybe Condition.Type.Condition
gate cr = case cr of
  CombatRestriction.CantAttack _ c -> c
  CombatRestriction.CantBlock _ c -> c
  CombatRestriction.CantAttackAlone _ c -> c

-- The shared walk behind all three questions above, over the restrictions
-- `select` keeps.
--
-- A set of ids and not a per-creature predicate: the caller asks this once per
-- declaration pass and then tests against the answer, where a predicate would
-- walk the whole battlefield per candidate and make the pass quadratic (#200).
-- `candidates` is the caller's chosen-from set (CR 508.1a for attacking, CR
-- 509.1a for blocking).
--
-- WHAT THE CALLER DOES WITH THE ANSWER is the caller's, and the two things done
-- with it are not interchangeable. `cantAttack` and `cantBlock` name creatures
-- that are in no legal declaration at all, so their sets are subtracted from the
-- candidate list before anything else runs; that also keeps CR 508.1d's and CR
-- 509.1c's maximizations honest, since a creature that cannot act can obey no
-- requirement. `cantAttackAlone` names creatures that are in SOME legal
-- declaration, so its set must stay on the candidate list and be asked of the
-- finished declaration instead -- subtracting it would forbid the very
-- declaration CR 508.1c's Example calls legal.
restricted :: (CombatRestriction.CombatRestriction -> Maybe Affected.Affected) -> [ObjectId] -> GameState -> Set ObjectId
restricted select candidates gs =
  let -- Hoisted out of the walk as AttackRequirement.instances hoists them, and
      -- both unforced until some permanent actually declares a restriction.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> []
        Just face -> case Face.combatRestrictions face of
          -- Every permanent in almost every game.
          [] -> []
          restrictions ->
            -- The same two ability losses AttackRequirement.instances asks
            -- about: CR 305.7's basic-land subtype set, and CR 604.2 against a
            -- CR 613.1f layer-6 removal. Why CR 613.6 cannot rescue a
            -- restriction that has started to apply is argued in
            -- BlockRequirement.instances.
            if (null setEffs || Projection.liveGiven setEffs Set.empty source gs)
              && not (removed source)
              then concatMap (fromRestriction source) restrictions
              else []
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection -- the opposite of
      -- Projection.affects's callers inside the layer fold, which read
      -- characteristics as of their own layer.
      named source affected creature =
        Projection.affects
          source
          creature
          affected
          (Projection.project creature gs)
          gs
      -- CR 508.1c / CR 509.1b's second clause. A gate that HOLDS lifts the
      -- restriction, so the creature is in none of the sets above; one that does
      -- not leaves it in force, which is why an ungated restriction is False
      -- here.
      --
      -- Evaluated once per RESTRICTION and not per candidate, because the
      -- clause belongs to the ability rather than to the creature it names: CR
      -- 109.5 fixes the "you" inside it as the SOURCE's controller, and
      -- Filter.IsSource names the source -- which is what makes Blind-Spot
      -- Giant's "another Giant" exclude the Giant printing the sentence.
      --
      -- Projection.fullView, matching the affected set above (CR 613.11). The
      -- source is on the battlefield by construction, so no CR 608.2h last
      -- known information is in play.
      lifted source restriction = case gate restriction of
        Nothing -> False
        Just condition ->
          Condition.holds
            (Projection.fullView gs)
            (Filter.MkContext (Projection.controllerOf source gs) (Just source))
            gs
            source
            condition
      fromRestriction source restriction = case select restriction of
        Nothing -> []
        Just affected
          | lifted source restriction -> []
          | otherwise -> filter (named source affected) candidates
   in Set.fromList (concatMap fromPermanent (Set.toList (GameState.battlefield gs)))

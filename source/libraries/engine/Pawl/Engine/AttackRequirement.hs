-- CR 508.1d / 613.11: the continuous effects that REQUIRE an attack. The twin
-- of Pawl.Engine.BlockRequirement on the other side of the combat phase, and
-- one of the modules on the axis CR 613.11 reaches past the layer system. None
-- is a layer, and Pawl.Engine.Projection sees none of them.
--
-- The only reader of Pawl.Types.AttackRequirement. Pawl.Engine.Combat asks for
-- requirement INSTANCES -- bare creature ids -- and never learns which card
-- produced one.
module Pawl.Engine.AttackRequirement where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)

-- CR 508.1d: every requirement in force right now, INSTANTIATED as the
-- creatures the active player is required to declare as attackers.
--
-- A set of ids and not a count, because CR 508.1d counts requirements being
-- OBEYED by a particular declaration, so each one must stay identifiable
-- against the declaration it is checked against. One instance per CREATURE, not
-- per ability: CR 508.1d checks each creature the active player controls, so
-- one Curse over three able creatures is three requirements. Two Curses over
-- the SAME creature collapse to one instance and cost no answer: a declaration
-- obeys both or neither.
--
-- `candidates` is CR 508.1a's chosen-from set, and it carries the "if able" of
-- "attacks each combat if able". Passed IN rather than computed here, as
-- BlockRequirement takes its `able` predicate, so this module never learns the
-- restrictions. Pruning by it changes no answer -- a creature that cannot
-- attack attacks in no legal declaration -- but it makes the instance set CR
-- 508.1d's maximum rather than an upper bound on it.
--
-- No `able` predicate BESIDE the candidate list, where the blocking twin has
-- one: there, CR 509.1b's restrictions are pairwise (flying, fear) and cannot
-- be decided per blocker. Every attacking restriction pawl models today is per
-- creature and already inside Combat.canAttack. A set-shaped restriction would
-- break that (#533).
instances :: [ObjectId] -> GameState -> Set ObjectId
instances candidates gs =
  let -- Hoisted out of the walk as BlockRequirement.instances hoists them, and
      -- both unforced until some permanent actually declares a requirement.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> []
        Just face -> case Face.attackRequirements face of
          -- Every permanent in almost every game.
          [] -> []
          requirements ->
            -- The same two ability losses BlockRequirement.instances asks
            -- about: CR 305.7's basic-land subtype set, and CR 604.2 against a
            -- CR 613.1f layer-6 removal. Why CR 613.6 cannot rescue a
            -- requirement that has started to apply is argued there.
            if (null setEffs || Projection.liveGiven setEffs Set.empty source gs)
              && not (removed source)
              then concatMap (fromRequirement source) requirements
              else []
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection -- the opposite of
      -- Projection.affects's callers inside the layer fold, which read
      -- characteristics as of their own layer.
      named source requirement creature =
        Projection.affects
          source
          creature
          (AttackRequirement.subject requirement)
          (Projection.project creature gs)
          gs
      fromRequirement source requirement = filter (named source requirement) candidates
   in Set.fromList (concatMap fromPermanent (Set.toList (GameState.battlefield gs)))

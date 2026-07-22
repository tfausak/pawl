module Pawl.Type.Combat where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Pawl.Type.AttackTarget (AttackTarget)
import Pawl.Type.ObjectId (ObjectId)

-- The current combat (CR 508/509). Cleared at end of combat (CR 511).
--
-- A record rather than flat GameState fields because combat state has a
-- LIFETIME the other fields do not -- one combat phase -- and something has to
-- reset it as a unit.
data Combat = MkCombat
  { attackers :: Map ObjectId AttackTarget,
    -- A Set per attacker, and never a single blocker: multi-blocking is legal
    -- from day one, and fixed arity is the recurring root cause (design doc
    -- §2.11).
    --
    -- UNORDERED, deliberately. Damage assignment order was REMOVED from the
    -- game -- the rules glossary lists it as Obsolete, CR 509.2 is now just "the
    -- active player gets priority", and CR 510.1c divides damage among blockers
    -- "as its controller chooses among them" with no ordering and no
    -- lethal-in-order rule. Storing an order would model a rule that does not
    -- exist and invite code enforcing a constraint the game does not have.
    -- Lethal-in-order survives only inside trample (CR 702.19b), where it
    -- belongs to that keyword, and arrives with M2.
    blockers :: Map ObjectId (Set ObjectId),
    -- CR 510.4: which attackers and blockers had first strike or double strike as
    -- the FIRST combat damage step began. Nothing while that step has not
    -- happened; Just once it has, so the second combat damage step knows who is
    -- excluded ("had neither...") and the step router knows a second step already
    -- ran. Reset at CR 511 (end of combat).
    --
    -- Stored, not re-derived: layer 6 means "had it then" and "has it now" come
    -- apart, and the CR 510.3 window between the two steps can change a keyword.
    -- Damage.dealCombatDamage reads this snapshot for CR 510.4's "had neither ...
    -- as the first step began" and reads DOUBLE strike live, which is what CR
    -- 510.4 says verbatim.
    struckFirst :: Maybe (Set ObjectId)
  }
  deriving (Eq, Ord, Show)

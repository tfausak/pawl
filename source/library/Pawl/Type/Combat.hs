module Pawl.Type.Combat where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Pawl.Type.AttackTarget (AttackTarget)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

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
    struckFirst :: Maybe (Set ObjectId),
    -- CR 506.2/506.2a: the one player being attacked this combat phase. Chosen as
    -- a turn-based action immediately after the beginning of combat step begins
    -- (CR 703.4h, CR 507.1) by Pawl.Combat.chooseDefender.
    --
    -- Nothing before that action has run, and again once Pawl.Combat.clearCombat
    -- has run. The RULES scope the designation to the combat phase -- CR 506.2's
    -- sentences all begin "During the combat phase" -- and CR 703.4h makes the
    -- choice per beginning-of-combat step, so a turn with a second combat phase
    -- (CR 500.8 is what adds one) chooses again rather than inheriting.
    --
    -- pawl's lifetime is one step SHORTER than the rules': clearCombat runs from
    -- Engine.runTurnBasedActions' end of combat step arm, i.e. as that step
    -- BEGINS, so the field reads Nothing for the whole of a step that CR 511.3's
    -- second sentence still places inside the combat phase (#180).
    --
    -- Nothing also means NO ATTACK IS POSSIBLE, which is the right answer and not
    -- a fallback: a turn whose active player has left the game (CR 800.4j) never
    -- performs the action.
    --
    -- Maybe PlayerId, not a set. CR 802 (attack multiple players) is the option
    -- that makes several players defenders at once, and CR 802.4 then has each of
    -- them declare blockers in APNAP order; neither is available here, because
    -- pawl has no options concept to read one from (#175). This field becomes a
    -- set when that arrives.
    defender :: Maybe PlayerId
  }
  deriving (Eq, Ord, Show)

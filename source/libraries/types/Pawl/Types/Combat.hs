module Pawl.Types.Combat where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | The current combat (CR 508/509). Cleared as the end of combat step ENDS
-- (CR 511.3), not as it begins -- so every field here reads live for the whole of
-- that step, which is what "target attacking creature" (Kill Shot) cast at
-- CR 511.1's priority depends on.
--
-- A record rather than flat GameState fields because combat state has a
-- LIFETIME the other fields do not -- one combat phase -- and something has to
-- reset it as a unit.
data Combat = MkCombat
  { attackers :: Map.Map ObjectId.ObjectId AttackTarget.AttackTarget,
    -- | A Set per attacker, and never a single blocker: multi-blocking is legal
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
    blockers :: Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId),
    -- | CR 510.4: which attackers and blockers had first strike or double strike as
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
    struckFirst :: Maybe (Set.Set ObjectId.ObjectId),
    -- | CR 506.4: who controlled each combatant AS IT JOINED combat -- the
    -- comparand Pawl.Engine.Combat.removeChanged needs to answer "if its
    -- controller changes". Keyed by the creature, so one map covers attackers
    -- and blockers alike; written by declareAttackers and declareBlockers, the
    -- only two things that put a creature into this record.
    --
    -- Stored, not re-derived, for the same reason struckFirst is: control is
    -- DERIVED (layer 2, Projection.controllerOf), so "who controls it now" and
    -- "who controlled it then" come apart the moment an effect grants control,
    -- and the rule is about the difference between them. Nothing else in the
    -- game state remembers the earlier value.
    --
    -- Not read off the ACTIVE PLAYER (attackers) and Combat.defender (blockers)
    -- instead, which CR 508.1k and CR 509.1g would license today: that answers a
    -- DIFFERENT question -- "is an attacking or defending player still its
    -- controller?" -- which coincides with this one only while there is exactly
    -- one of each. CR 802 is what makes several players defenders at once, and
    -- pawl has no options concept to read it from (#175). CR 506.4 asks about the
    -- permanent's own controller either way, so the snapshot states the rule the
    -- rule states, for one map.
    joinedUnder :: Map.Map ObjectId.ObjectId PlayerId.PlayerId,
    -- | WHAT has been attacked this combat phase: the CR 508.1b target announced
    -- for each creature "declared as [an attacker] or put onto the battlefield
    -- attacking" (CR 508.8). Written by Pawl.Engine.Combat.declareAttackers and
    -- Pawl.Engine.Combat.putOntoBattlefieldAttacking, the two things that can do either,
    -- and by nothing else.
    --
    -- SEPARATE from `attackers`, and monotone within the combat phase, because
    -- both rules that read it ask a HISTORICAL question and that map is current
    -- state. CR 508.1k is the rule that separates them: a declared creature
    -- "remains an attacking creature until it's removed from combat", so CR
    -- 506.4's removal ends its attacking and leaves its having been declared
    -- untouched. Reading `attackers` instead answered CR 508.8 wrong for a lone
    -- attacker that a Ray of Command took (CR 506.4's control-change clause) or
    -- that regenerated (CR 701.19a) during the declare attackers step -- both of
    -- which delete the entry, and neither of which un-declares anything.
    --
    -- The two readers:
    --
    --   * CR 508.8's skip -- "If no creatures are declared as attackers or put
    --     onto the battlefield attacking, skip the declare blockers and combat
    --     damage steps" -- is EMPTINESS (Pawl.Engine.Combat.skipEmptyCombat). That is the
    --     same question as "did any creature join", not a weaker one: CR 508.1b
    --     gives every joining creature exactly one target, and both writers add
    --     it in the same update that puts the creature into `attackers`, so this
    --     is non-empty exactly when one joined.
    --   * "only if you've been attacked this step" (Rally the Troops) is
    --     MEMBERSHIP of OfPlayer (Pawl.Engine.Cast.attackedThisStep). Eightfold Maze's
    --     ruling is why that one cannot be emptiness: "If all the attacking
    --     creatures attack your planeswalkers, you can't cast Eightfold Maze. To
    --     cast it, a creature needs to have attacked _you_."
    --
    -- A set of TARGETS and not of attacker ids: neither reader asks WHICH
    -- creature, and keying by the creature would shadow `attackers`' key set and
    -- invite the two to be confused.
    --
    -- Its lifetime is this record's, which is exact rather than lucky. CR 508.8
    -- scopes the question to one declare attackers step; a combat phase has
    -- exactly one of those, and Pawl.Engine.Combat.clearCombat resets the record as each
    -- end of combat step ends (CR 511.3), so a CR 500.8 additional combat phase
    -- asks the question again from empty.
    attacked :: Set.Set AttackTarget.AttackTarget,
    -- | CR 506.2/506.2a: the one player being attacked this combat phase. Chosen as
    -- a turn-based action immediately after the beginning of combat step begins
    -- (CR 703.4h, CR 507.1) by Pawl.Engine.Combat.chooseDefender.
    --
    -- Nothing before that action has run, and again once Pawl.Engine.Combat.clearCombat
    -- has run. The RULES scope the designation to the combat phase -- CR 506.2's
    -- sentences all begin "During the combat phase" -- and CR 703.4h makes the
    -- choice per beginning-of-combat step, so a turn with a second combat phase
    -- (CR 500.8 is what adds one) chooses again rather than inheriting.
    --
    -- That lifetime is exact: Engine.runStep clears the record as the end of
    -- combat step ENDS (CR 511.3), and CR 511.3's second sentence puts the phase
    -- boundary immediately after that step -- so the field is Just for precisely
    -- the combat phase and Nothing outside it.
    --
    -- Nothing also means NO ATTACK IS POSSIBLE, which is the right answer and not
    -- a fallback: a turn whose active player has left the game never performs the
    -- action. CR 800.4j is only why there is no active player to perform it --
    -- that rule is about PRIORITY and says nothing about turn-based actions.
    -- CR 800.4h is the one that speaks to this action: the defending player is a
    -- choice a RULE (CR 507.1, CR 703.4h) requires of a player who has left, and
    -- CR 800.4h gives it to the next player in turn order. pawl skips it instead,
    -- resolving the choice silently rather than reassigning it (#181).
    --
    -- The divergence is UNOBSERVABLE, not vacuous -- there really is a choice
    -- among the departed player's surviving opponents. It cannot be seen because
    -- the field would be written and never read: CR 506.2's first sentence makes
    -- the attacking player the active player, and after CR 800.4a a departed
    -- player controls no creature, so no creature can attack under any defending
    -- player CR 800.4h would install.
    --
    -- Maybe PlayerId, not a set. CR 802 (attack multiple players) is the option
    -- that makes several players defenders at once, and CR 802.4 then has each of
    -- them declare blockers in APNAP order; neither is available here, because
    -- pawl has no options concept to read one from (#175). This field becomes a
    -- set when that arrives.
    defender :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)

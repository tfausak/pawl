module Pawl.Types.Combat where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | The current combat (CR 508/509). Cleared as the end of combat step ENDS
-- (CR 511.3), not as it begins, so every field here reads live for the whole of
-- that step.
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
    -- game -- the rules glossary lists it as Obsolete, and CR 510.1c divides
    -- damage among blockers as its controller chooses, with no ordering.
    -- Storing an order would model a rule that does not exist. Lethal-in-order
    -- survives only inside trample (CR 702.19b), where it belongs to that
    -- keyword.
    blockers :: Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId),
    -- | CR 510.4: which attackers and blockers had first strike or double strike
    -- as the FIRST combat damage step began. Nothing while that step has not
    -- happened; Just once it has, so the second combat damage step knows who is
    -- excluded and the step router knows a second step already ran. Reset at
    -- CR 511 (end of combat).
    --
    -- Stored, not re-derived: layer 6 means "had it then" and "has it now" come
    -- apart, and the CR 510.3 window between the two steps can change a keyword.
    -- Damage.dealCombatDamage reads this snapshot for the exclusion and reads
    -- DOUBLE strike live, which is what CR 510.4 says verbatim.
    struckFirst :: Maybe (Set.Set ObjectId.ObjectId),
    -- | CR 506.4: who controlled each combatant AS IT JOINED combat -- the
    -- comparand for "if its controller changes". Keyed by the creature, so one
    -- map covers attackers and blockers alike.
    --
    -- Stored, not re-derived, for the same reason struckFirst is: control is
    -- DERIVED (layer 2), so "who controls it now" and "who controlled it then"
    -- come apart the moment an effect grants control, and the rule is about the
    -- difference between them. Nothing else in the game state remembers the
    -- earlier value.
    --
    -- Not read off the ACTIVE PLAYER (attackers) and Combat.defender (blockers)
    -- instead, which CR 508.1k and CR 509.1g would license today: that answers a
    -- DIFFERENT question -- "is an attacking or defending player still its
    -- controller?" -- which coincides with this one only while there is exactly
    -- one of each. CR 802 is what makes several players defenders at once, and
    -- pawl has no options concept to read it from (#175).
    joinedUnder :: Map.Map ObjectId.ObjectId PlayerId.PlayerId,
    -- | WHAT has been attacked this combat phase: the CR 508.1b target announced
    -- for each creature declared as an attacker or put onto the battlefield
    -- attacking (CR 508.8). Written by Pawl.Engine.Combat.declareAttackers and
    -- Pawl.Engine.Combat.putOntoBattlefieldAttacking, and by nothing else.
    --
    -- SEPARATE from `attackers`, and monotone within the combat phase, because
    -- both rules that read it ask a HISTORICAL question and that map is current
    -- state. CR 508.1k is the rule that separates them: a declared creature
    -- remains an attacking creature until it's removed from combat, so CR
    -- 506.4's removal ends its attacking and leaves its having been declared
    -- untouched.
    --
    -- ONE reader: CR 508.8's skip of the declare blockers and combat damage
    -- steps, as EMPTINESS (Pawl.Engine.Combat.skipEmptyCombat). That is the same
    -- question as "did any creature join", not a weaker one: CR 508.1b gives
    -- every joining creature exactly one target, and both writers add it in the
    -- same update that puts the creature into `attackers`.
    --
    -- A set of TARGETS and not of attacker ids: the reader does not ask WHICH
    -- creature, and keying by the creature would shadow `attackers`' key set and
    -- invite the two to be confused.
    --
    -- Its lifetime is this record's, which is exact rather than lucky. CR 508.8
    -- scopes the question to one declare attackers step, so a CR 500.8
    -- additional combat phase asks the question again from empty.
    attacked :: Set.Set AttackTarget.AttackTarget,
    -- | CR 508.3b / 508.4: the targets a creature was DECLARED attacking.
    --
    -- A SUBSET of `attacked` above but not a difference from it -- both fields
    -- are unions, and a target attacked BOTH ways in one step is in both. What
    -- separates them is the writer: declareAttackers adds to both,
    -- putOntoBattlefieldAttacking adds only to `attacked`.
    --
    -- The two are different questions and the rules answer them differently.
    -- CR 508.8 counts both, so the skip reads `attacked`. But CR 508.4 is
    -- emphatic that a creature put onto the battlefield attacking never
    -- "attacked", for the purposes of trigger events AND EFFECTS -- and a
    -- printed casting restriction is an effect, not a trigger. CR 508.3b is the
    -- same principle worked out for the trigger case only, so it is
    -- corroboration rather than authority.
    --
    -- So anything asking whether a PLAYER was attacked reads this one:
    -- Pawl.Engine.Cast.attackedThisStep, for Rally the Troops' "only if you've
    -- been attacked this step".
    --
    -- Same lifetime and same never-cleared posture as `attacked`, for that
    -- field's reasons.
    declaredAttacked :: Set.Set AttackTarget.AttackTarget,
    -- | CR 506.7b: has this combat phase's declare blockers step declared
    -- blockers? The boundary "only during combat after blockers are declared"
    -- names, and the sole reader is
    -- Pawl.Engine.Turn.afterBlockersDeclared (Curtain of Light on the casting
    -- side, Trap Runner on the activation side).
    --
    -- CR 506.7b's own words are "after the declare blockers step begins", which
    -- is a hair earlier than the CR 509.1 turn-based action this field is
    -- written by. Nothing can tell them apart: CR 509.2 gives the active player
    -- priority only once that action is finished, so no cast or activation ever
    -- sees the gap.
    --
    -- Written at the TOP of Pawl.Engine.Combat.declareBlockers, ahead of its
    -- own "nothing is attacking" short-circuit, because CR 506.7b opens the
    -- window "regardless of whether any blockers are actually declared" -- and a
    -- combat whose attackers were all removed after CR 508.8 asked its question
    -- still runs the step.
    --
    -- Stored rather than derived, and no other field answers it. CR 506.7f asks
    -- about a step that did NOT happen: a combat phase whose declare blockers
    -- step is skipped admits the spell nowhere in that phase, its end of combat
    -- step included, and the phase the game is IN cannot say that. This
    -- record's lifetime is what makes the answer exact -- CR 511.3's reset
    -- re-arms it, which is what CR 506.7c's "any of them" wants from a CR 500.8
    -- second combat phase.
    blockersDeclared :: Bool,
    -- | CR 506.2/506.2a: the one player being attacked this combat phase. Chosen
    -- as a turn-based action immediately after the beginning of combat step
    -- begins (CR 703.4h, CR 507.1).
    --
    -- Nothing before that action has run, and again once combat is cleared. The
    -- RULES scope the designation to the combat phase (CR 506.2), and CR 703.4h
    -- makes the choice per beginning-of-combat step, so a turn with a second
    -- combat phase (CR 500.8) chooses again rather than inheriting. CR 511.3
    -- puts the phase boundary immediately after the end of combat step, so the
    -- field is Just for precisely the combat phase.
    --
    -- Nothing also means NO ATTACK IS POSSIBLE, which is the right answer and
    -- not a fallback: a turn whose active player has left the game never
    -- performs the action. CR 800.4h would give that choice to the next player
    -- in turn order; pawl skips it instead, resolving it silently rather than
    -- reassigning it (#181). The divergence is UNOBSERVABLE, not vacuous: the
    -- field would be written and never read, because CR 506.2 makes the
    -- attacking player the active player and after CR 800.4a a departed player
    -- controls no creature.
    --
    -- Maybe PlayerId, not a set. CR 802 (attack multiple players) is the option
    -- that makes several players defenders at once, and CR 802.4 then has each of
    -- them declare blockers in APNAP order; neither is available here, because
    -- pawl has no options concept to read one from (#175). This field becomes a
    -- set when that arrives.
    defender :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)

module Pawl.Types.CombatRestriction where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition

-- | CR 508.1c / CR 509.1b: one printed COMBAT RESTRICTION -- an effect saying a
-- creature can't attack, or can't attack unless some condition is met -- or the
-- sentence CR 509.1b writes with "block" in place of "attack". Pacifism states
-- both in one line, and prints one of the first two arms below.
--
-- The FIFTH carrier of a printed static ability, alongside
-- Pawl.Types.StaticAbility, Pawl.Types.PlayerStaticAbility,
-- Pawl.Types.BlockRequirement and Pawl.Types.AttackRequirement.
-- Pawl.Types.BlockRequirement's header argues why neither of the first two can
-- hold a REQUIREMENT, and every step of that argument holds here unchanged:
-- CR 613.1's layers compute an OBJECT's characteristics and "can't attack" is
-- not one of them, and PlayerScope names PLAYERS where this names a creature.
--
-- What is different is that this is ONE type where the requirements are two, and
-- the reason is the AXIS. CR 508.1d and CR 509.1c each imply a subject and an
-- object, and the two requirement carriers collapse opposite ones -- a blocking
-- requirement carries the attacker to be blocked and no subject, an attacking
-- requirement carries the subject and no object. A restriction
-- carries only the SUBJECT on every arm: Pacifism's two halves are the same
-- Affected twice, so the only thing distinguishing them is which declaration
-- they forbid. Splitting them would copy the requirements' shape without the
-- reason for it.
--
-- The arms differ in SHAPE rather than in axis, and there are two shapes. The
-- first two are answered about a creature ALONE, so the reader may apply them to
-- CR 508.1a's and CR 509.1a's candidate list and never look at a declaration.
-- CantAttackAlone cannot be: it is a fact about the whole set of creatures
-- declared as attackers, so it is answered against the finished declaration
-- instead. Both shapes are CR 508.1c restrictions all the same -- that rule's own
-- Example is a "can't attack alone" board -- so they share the type rather than
-- splitting into a second carrier.
--
-- A restriction a player may PAY THROUGH is one of CR 508.1c's all the same
-- (Ghostly Prison), but it rides Pawl.Types.AttackCost, the SIXTH carrier. The
-- split is pawl's, not the rules': this type's answer is a SET OF CREATURES that
-- may not attack, and CantAttack takes its subject off CR 508.1a's candidate
-- list entirely, where a taxed creature has to stay on it. A cost is also a
-- thing to be PAID rather than a fact to be read, and CR 508.1d makes paying it
-- optional, so it could not be a Condition.
--
-- The axis is missing rather than collapsed, and the missing capability is
-- named: an attacking restriction with an object (Crown-Hunter Hireling, Armored
-- Galleon) is one whose CONDITION is about the player CR 508.1b names per
-- creature, and the condition below cannot name that player (#620). A blocking restriction with an object is
-- what CR 702.9b's evasion keywords already are -- carried on the ATTACKER as a
-- keyword, never here.
--
-- Open-half card data, classified rather than identified:
-- Pawl.Engine.CombatRestriction is the only module that may case on it. Casing
-- here is casing on which rulebook declaration a restriction forbids and in what
-- shape, not on an effect's identity.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture all five siblings take -- so a Pacifism leaving the battlefield lifts
-- its restriction with nothing to unwind. The gate is re-read on the same
-- schedule: CR 508.1 and CR 509.1 make the declaration a SEQUENCE OF STEPS, of
-- which CR 508.1c and CR 509.1b are one, so the only moment a gate's answer has
-- to be right is the moment it is asked, and a gate that stops holding
-- re-imposes the restriction with nothing to unwind either. CR 509.1b's note
-- that an evasion
-- ability gained after a legal block does not affect that block is the rules
-- saying the same thing.
--
-- The SECOND field of each arm is that gate: the condition the creature can't
-- attack (or block) UNLESS -- Blind-Spot Giant's "unless you control another
-- Giant". Nothing is the unconditional restriction (Pacifism), which is not the
-- same as a condition that never holds: the two would answer alike today, but
-- only one of them is what the card says. A Condition states the gate because
-- CR 508.1c's condition is a predicate over game STATE, which is the type
-- Pawl.Types.Condition already is.
--
-- The "you" inside the condition is CR 109.5's: the controller of the permanent
-- the restriction is printed on, which is not necessarily the controller of the
-- creature it restricts. Blind-Spot Giant is its own source and cannot tell them
-- apart; a conditional Aura would.
data CombatRestriction
  = -- | CR 508.1c: these creatures can't attack, unless the gate holds. Pacifism
    -- (ungated) and Blind-Spot Giant (gated) are the pool's printed attacking
    -- restrictions that are not CR 702.3b's defender keyword, which stays a
    -- Keyword because rule 702 is part of the rulebook and a keyword's meaning
    -- is the closed half's to know.
    CantAttack Affected.Affected (Maybe Condition.Condition)
  | -- | CR 509.1b: these creatures can't block, unless the gate holds.
    -- Pacifism's second half and Blind-Spot Giant's are the pool's only printed
    -- blocking restrictions; every other one today is an evasion keyword on the
    -- ATTACKER, which restricts being blocked rather than blocking.
    CantBlock Affected.Affected (Maybe Condition.Condition)
  | -- | CR 508.1c together with CR 506.5: these creatures can't be the ONLY
    -- creature declared as an attacker, unless the gate holds. Bonded Construct
    -- ("This creature can't attack alone") is the pool's printing, and CR
    -- 508.1c's own Example is a board of two of them.
    --
    -- Not CantAttack with a Condition, and the difference is not expressible as
    -- one: CR 506.5's "alone" is a fact about the DECLARATION being written, and
    -- a Condition is a predicate over game state, which at CR 508.1c time cannot
    -- see a declaration that has not been made. That is also why this arm exists
    -- rather than a cleverer Affected -- Affected names creatures, and no set of
    -- creatures says "however many of you there are".
    --
    -- The gate beside it is the same "unless" every arm carries, and is about
    -- game state as usual: nothing in the pool prints a gated one.
    CantAttackAlone Affected.Affected (Maybe Condition.Condition)
  deriving (Eq, Ord, Show)

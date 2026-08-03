module Pawl.Types.CombatRestriction where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition

-- | CR 508.1c / CR 509.1b: one printed COMBAT RESTRICTION -- an "effect that says a
-- creature can't attack, or that it can't attack unless some condition is met",
-- or the sentence CR 509.1b writes with "block" in place of "attack". Pacifism's
-- "Enchanted creature can't attack or block" is one line stating both, and prints
-- one of each arm below.
--
-- The FIFTH carrier of a printed static ability, alongside
-- Pawl.Types.StaticAbility, Pawl.Types.PlayerStaticAbility,
-- Pawl.Types.BlockRequirement and Pawl.Types.AttackRequirement.
-- Pawl.Types.BlockRequirement's header argues at length why neither of the first
-- two can hold a REQUIREMENT, and every step of that argument holds here
-- unchanged: CR 613.1 makes the seven layers a machine for computing an OBJECT's
-- characteristics, and "can't attack" is not one of them, so no Modification
-- could express it; and PlayerScope names PLAYERS, where this names a creature.
-- That argument is not repeated; only what is different is.
--
-- What is different is that this is ONE type where the requirements are two.
-- Pawl.Types.AttackRequirement's header gives the reason those are two, and it is
-- the AXIS: CR 508.1d and CR 509.1c each imply a subject and an object, and the
-- two carriers collapse opposite ones -- a blocking requirement carries the
-- attacker to be blocked and no subject, an attacking requirement carries the
-- subject and no object. A restriction carries only the SUBJECT on BOTH sides:
-- Pacifism's two halves are the same Affected twice -- and Blind-Spot Giant's are
-- the same Affected and the same gate twice -- so the only thing that
-- distinguishes them is which declaration they forbid. Splitting them into two
-- carriers would copy the requirements' shape without the reason for it.
--
-- A restriction a player may PAY THROUGH is one of CR 508.1c's all the same --
-- Ghostly Prison's "creatures can't attack you unless their controller pays
-- {2} ..." is the second arm of the parenthetical below -- but it rides
-- Pawl.Types.AttackCost, the SIXTH carrier, rather than this type. The split is
-- pawl's, not the rules': this type's answer is a SET OF CREATURES that may not
-- attack, and CantAttack takes its subject off CR 508.1a's candidate list
-- entirely, where a taxed creature has to stay on it. That carrier's header
-- argues it in full.
--
-- The axis is missing rather than collapsed, and the missing capability is
-- named: an attacking restriction with an object (Crown-Hunter Hireling's "can't
-- attack unless defending player is the monarch", Armored Galleon's "unless
-- defending player controls an Island") is a restriction whose CONDITION is
-- about a player CR 508.1b names per creature, and the condition below cannot
-- name that player (#620). A blocking restriction with an object is what CR
-- 702.9b's evasion keywords already are -- carried on the ATTACKER as a keyword,
-- never here. A restriction whose subject is a SET ("no more than one creature
-- can attack each combat", Silent Arbiter) is a different shape again and is not
-- representable here (#533).
--
-- BOTH halves of each rule's parenthetical are stated. CR 508.1c and CR 509.1b
-- each read "effects that say a creature can't attack, or that it can't attack
-- unless some condition is met": the first half is an arm with no condition
-- (Pacifism), the second an arm carrying one (Blind-Spot Giant). The one shape of
-- the second half this type does NOT carry is the condition "unless a player pays
-- a cost", which is Pawl.Types.AttackCost's -- a cost is a thing to be PAID and
-- not a fact to be read, and CR 508.1d's third sentence makes paying it optional,
-- so it cannot be a Condition.
--
-- Open-half card data, classified rather than identified: Pawl.Engine.CombatRestriction
-- is the only module that may case on it, exactly as Pawl.Engine.Cast is the only
-- reader of Pawl.Types.CastingRestriction. Casing here is casing on a
-- RESTRICTION's classification -- which of two rulebook declarations it forbids
-- -- and not on an effect's identity.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture all five siblings take -- so a Pacifism leaving the battlefield lifts
-- its restriction with nothing to unwind. The gate is re-read on the same
-- schedule and for the same reason. CR 508.1 and CR 509.1 make the declaration a
-- SEQUENCE OF STEPS ("if at any point during the declaration of attackers the
-- active player is unable to comply with any of the steps listed below, the
-- declaration is illegal"), of which CR 508.1c and CR 509.1b are one -- "the
-- active player CHECKS each creature they control". So the only moment a gate's
-- answer has to be right is the moment it is asked, and a gate that stops holding
-- re-imposes the restriction with nothing to unwind either. CR 509.1b's own note
-- that an evasion ability gained "after a legal block has been declared ...
-- doesn't affect that block" is the rules saying the same thing about the one
-- restriction they wrote a note for.
--
-- The SECOND field of each arm is that gate: the condition the creature can't
-- attack (or block) UNLESS -- Blind-Spot Giant's "unless you control another
-- Giant". Nothing is the unconditional restriction, which is Pacifism and is not
-- the same as a condition that never holds: the two would answer alike today, but
-- only one of them is what the card says. A Condition is what states the gate
-- because CR 508.1c's "some condition is met" is a predicate over game STATE,
-- which is the type Pawl.Types.Condition already is.
--
-- The "you" inside the condition is CR 109.5's: the controller of the permanent
-- the restriction is printed on, which is not necessarily the controller of the
-- creature it restricts. Blind-Spot Giant is its own source and cannot tell them
-- apart; a conditional Aura would.
data CombatRestriction
  = -- | CR 508.1c: these creatures can't attack, unless the gate holds. Pacifism's
    -- first half (ungated) and Blind-Spot Giant's (gated) -- the pool's first two
    -- printed attacking restrictions that are not CR 702.3b's defender keyword,
    -- which stays a Keyword because rule 702 is part of the rulebook and a
    -- keyword's meaning is the closed half's to know.
    CantAttack Affected.Affected (Maybe Condition.Condition)
  | -- | CR 509.1b: these creatures can't block, unless the gate holds. Pacifism's
    -- second half and Blind-Spot Giant's, and the pool's only printed blocking
    -- restrictions: every other one today is an evasion keyword on the ATTACKER
    -- (flying, fear, landwalk), which restricts being blocked rather than
    -- blocking.
    CantBlock Affected.Affected (Maybe Condition.Condition)
  deriving (Eq, Ord, Show)

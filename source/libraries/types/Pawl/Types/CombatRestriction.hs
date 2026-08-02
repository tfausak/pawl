module Pawl.Types.CombatRestriction where

import qualified Pawl.Types.Affected as Affected

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
-- Pacifism's two halves are the same Affected twice, and the only thing that
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
-- The axis is missing rather than collapsed, and the missing capabilities are
-- named: an attacking restriction with an object (Crown-Hunter Hireling's "can't
-- attack unless defending player is the monarch") needs CR 508.1b's per-creature
-- announcement, which pawl's declaration does not have (#59, #461), as well as CR
-- 508.1c's condition clause (#534); and a blocking restriction with an
-- object is what CR 702.9b's evasion keywords already are -- carried on the
-- ATTACKER as a keyword, never here. A restriction whose subject is a SET
-- ("no more than one creature can attack each combat", Silent Arbiter) is a
-- different shape again and is not representable here (#533).
--
-- Only the FIRST half of each rule's parenthetical is stated. CR 508.1c and CR
-- 509.1b each read "effects that say a creature can't attack, or that it can't
-- attack unless some condition is met", and neither arm carries a condition, so
-- the second clause is unrepresentable HERE (#534). The one shape of it that has
-- a carrier is the condition "unless a player pays a cost", on
-- Pawl.Types.AttackCost.
--
-- Open-half card data, classified rather than identified: Pawl.Engine.CombatRestriction
-- is the only module that may case on it, exactly as Pawl.Engine.Cast is the only
-- reader of Pawl.Types.CastingRestriction. Casing here is casing on a
-- RESTRICTION's classification -- which of two rulebook declarations it forbids
-- -- and not on an effect's identity.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture all five siblings take -- so a Pacifism leaving the battlefield lifts
-- its restriction with nothing to unwind.
data CombatRestriction
  = -- | CR 508.1c: these creatures can't attack. Pacifism's first half, and the
    -- second printed attacking restriction in the pool after CR 702.3b's
    -- defender keyword -- which stays a Keyword, because rule 702 is part of the
    -- rulebook and a keyword's meaning is the closed half's to know.
    CantAttack Affected.Affected
  | -- | CR 509.1b: these creatures can't block. Pacifism's second half, and the
    -- first printed blocking restriction in the pool: every other one today is an
    -- evasion keyword on the ATTACKER (flying, fear, landwalk), which restricts
    -- being blocked rather than blocking.
    CantBlock Affected.Affected
  deriving (Eq, Ord, Show)

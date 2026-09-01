module Pawl.Types.AttackTargetKind where

-- | Which of CR 506.3's three attackable things an announcement names, with the
-- thing itself left out: Pawl.Types.AttackTarget stripped of its payload.
--
-- One arm per AttackTarget arm, and named alike, so a fourth attackable thing is
-- a compile error at both types at once. Pawl.Engine.Combat.attackTargetKind is
-- the mapping.
--
-- What wants the kind without the object is a restriction naming a CLASS of
-- announcement rather than a permanent -- Pawl.Types.CantAttackPlayer's `kinds`,
-- Vow of Flight's "can't attack you or planeswalkers you control".
--
-- Bounded and Enum for Pawl.Codec.AttackTargetKind's Arm.enum.
data AttackTargetKind
  = -- | CR 506.3: the defending player themselves.
    OfPlayer
  | -- | CR 306.6: a planeswalker that player controls.
    OfPlaneswalker
  | -- | CR 310.5: a battle that player protects (CR 310.9b).
    OfBattle
  deriving (Bounded, Enum, Eq, Ord, Show)

module Pawl.Types.CombatStep where

data CombatStep
  = BeginningOfCombat
  | DeclareAttackers
  | DeclareBlockers
  | CombatDamage
  | EndOfCombat
  deriving (Bounded, Enum, Eq, Ord, Show)

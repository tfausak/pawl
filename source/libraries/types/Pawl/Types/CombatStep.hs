module Pawl.Types.CombatStep where

data CombatStep
  = BeginningOfCombat
  | DeclareAttackers
  | DeclareBlockers
  | CombatDamage
  | EndOfCombat
  deriving (Eq, Ord, Show)

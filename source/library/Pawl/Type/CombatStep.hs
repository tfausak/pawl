module Pawl.Type.CombatStep where

data CombatStep
  = BeginningOfCombat
  | DeclareAttackers
  | DeclareBlockers
  | CombatDamage
  | EndOfCombat
  deriving (Eq, Ord, Show)

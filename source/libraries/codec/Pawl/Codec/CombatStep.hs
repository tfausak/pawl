-- | The @CombatStep ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.CombatStep where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.CombatStep as CombatStep

combatStepToJson :: CombatStep.CombatStep -> Value
combatStepToJson s = Json.nullary . Text.pack $ case s of
  CombatStep.BeginningOfCombat -> "BeginningOfCombat"
  CombatStep.DeclareAttackers -> "DeclareAttackers"
  CombatStep.DeclareBlockers -> "DeclareBlockers"
  CombatStep.CombatDamage -> "CombatDamage"
  CombatStep.EndOfCombat -> "EndOfCombat"

jsonToCombatStep :: Value -> Either Text CombatStep.CombatStep
jsonToCombatStep =
  Json.decodeNullary
    (Text.pack "CombatStep")
    [ (Text.pack "BeginningOfCombat", CombatStep.BeginningOfCombat),
      (Text.pack "DeclareAttackers", CombatStep.DeclareAttackers),
      (Text.pack "DeclareBlockers", CombatStep.DeclareBlockers),
      (Text.pack "CombatDamage", CombatStep.CombatDamage),
      (Text.pack "EndOfCombat", CombatStep.EndOfCombat)
    ]

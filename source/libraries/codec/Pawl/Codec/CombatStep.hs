module Pawl.Codec.CombatStep where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CombatStep as CombatStep

toJson :: CombatStep.CombatStep -> Value.Value
toJson s = Common.nullary $ case s of
  CombatStep.BeginningOfCombat -> "BeginningOfCombat"
  CombatStep.DeclareAttackers -> "DeclareAttackers"
  CombatStep.DeclareBlockers -> "DeclareBlockers"
  CombatStep.CombatDamage -> "CombatDamage"
  CombatStep.EndOfCombat -> "EndOfCombat"

fromJson :: Value.Value -> Either Text.Text CombatStep.CombatStep
fromJson =
  Common.decodeNullary
    "CombatStep"
    [ ("BeginningOfCombat", CombatStep.BeginningOfCombat),
      ("DeclareAttackers", CombatStep.DeclareAttackers),
      ("DeclareBlockers", CombatStep.DeclareBlockers),
      ("CombatDamage", CombatStep.CombatDamage),
      ("EndOfCombat", CombatStep.EndOfCombat)
    ]

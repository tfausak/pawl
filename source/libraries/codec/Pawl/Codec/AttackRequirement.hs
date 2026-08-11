module Pawl.Codec.AttackRequirement where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.AttackRequirement as AttackRequirement

-- | The key is "subject" and not "attacker": CR 508.1d's requirement names the
-- creatures REQUIRED to attack, where CR 509.1c's names the attacker to be
-- blocked. Same shape, opposite axis (Pawl.Types.AttackRequirement).
toJson :: AttackRequirement.AttackRequirement -> Value.Value
toJson ar =
  Common.object (Common.requiredPair "subject" Affected.toJson (AttackRequirement.subject ar))

fromJson :: Value.Value -> Either Text.Text AttackRequirement.AttackRequirement
fromJson value = do
  ps <- Common.asObject value
  a <- Common.field "subject" ps >>= Affected.fromJson
  pure (AttackRequirement.MkAttackRequirement a)

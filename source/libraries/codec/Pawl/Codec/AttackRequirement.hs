-- | The @AttackRequirement ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.AttackRequirement where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Affected (affectedToJson, jsonToAffected)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.AttackRequirement as AttackRequirement

-- The key is "subject" and not "attacker": CR 508.1d's requirement names the
-- creatures REQUIRED to attack, where CR 509.1c's names the attacker to be
-- blocked. Same shape, opposite axis (Pawl.Types.AttackRequirement).
attackRequirementToJson :: AttackRequirement.AttackRequirement -> Value
attackRequirementToJson ar =
  Json.jObject [(Text.pack "subject", affectedToJson (AttackRequirement.subject ar))]

jsonToAttackRequirement :: Value -> Either Text AttackRequirement.AttackRequirement
jsonToAttackRequirement value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "subject") ps >>= jsonToAffected
  pure (AttackRequirement.MkAttackRequirement a)

module Pawl.Codec.AttackCost where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.AttackCost as AttackCost

-- | "subject" is Pawl.Codec.AttackRequirement's key and names the same axis --
-- CR 508.1d's creatures, not CR 509.1c's attacker to be blocked.
-- "perAttacker" is not the card's whole cost: CR 508.1h multiplies it by how many
-- taxed attackers the declaration holds, so the key names one attacker's share.
toJson :: AttackCost.AttackCost -> Value.Value
toJson ac =
  Common.object
    [ Common.pair "subject" (Affected.toJson (AttackCost.subject ac)),
      Common.pair "perAttacker" (ManaCost.toJson (AttackCost.perAttacker ac))
    ]

fromJson :: Value.Value -> Either Text.Text AttackCost.AttackCost
fromJson value = do
  ps <- Common.asObject value
  subject <- Common.field "subject" ps >>= Affected.fromJson
  perAttacker <- Common.field "perAttacker" ps >>= ManaCost.fromJson
  pure (AttackCost.MkAttackCost subject perAttacker)

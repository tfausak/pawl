module Pawl.Codec.AttackCost where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.AttackCost as AttackCost

-- | "subject" is Pawl.Codec.AttackRequirement's key and names the same axis: the
-- creatures the effect is ABOUT, never Pawl.Codec.BlockRequirement's "attacker",
-- which is the object CR 509.1c's requirements carry instead.
-- "perAttacker" is not the card's whole cost: Ghostly Prison's own "for each"
-- repeats it once per taxed attacker before CR 508.1h totals the declaration, so
-- the key names one attacker's share.
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

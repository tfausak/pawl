module Pawl.Codec.AttackCost where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.AttackCost as AttackCost

-- | "subject" is Pawl.Codec.AttackRequirement's key and names the same axis:
-- the creatures the effect is ABOUT, never Pawl.Codec.BlockRequirement's
-- "attacker", which is the object CR 509.1c's requirements carry instead.
-- "perAttacker" is one attacker's share, not the card's whole cost -- a "for
-- each" repeats it per taxed attacker before CR 508.1h totals the declaration.
toJson :: AttackCost.AttackCost -> Value.Value
toJson ac =
  Value.object
    ( Common.requiredPair "subject" Affected.toJson (AttackCost.subject ac)
        <> Common.requiredPair "perAttacker" (Codec.encode ManaCost.codec) (AttackCost.perAttacker ac)
    )

fromJson :: Value.Value -> Either Text.Text AttackCost.AttackCost
fromJson value = do
  ps <- Common.asObject value
  subject <- Common.field "subject" ps >>= Affected.fromJson
  perAttacker <- Common.field "perAttacker" ps >>= Codec.decode ManaCost.codec
  pure (AttackCost.MkAttackCost subject perAttacker)

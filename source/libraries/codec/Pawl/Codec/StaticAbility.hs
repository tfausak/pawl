module Pawl.Codec.StaticAbility where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.Modification as Modification
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.StaticAbility as StaticAbility

-- | CR 613.6: the parts of one ability's effect travel together, so the wire
-- format is one affected set and an ARRAY of modifications -- never one entry
-- per layer.
--
-- The CR 604.2 "as long as" gate is OPTIONAL, and absent means unconditional:
-- every card written before it existed encodes byte-for-byte as it did. The CR
-- 604.2 override beside it -- "if this leaves the battlefield, this effect
-- continues until end of turn" -- is optional for the same reason, and absent
-- means the effect ends with its permanent.
toJson :: StaticAbility.StaticAbility -> Value.Value
toJson sa =
  Value.object
    ( Common.requiredPair "affected" (Codec.encode Affected.codec) (StaticAbility.affected sa)
        <> Common.optionalPair "condition" Nothing (Common.encodeMaybe (Codec.encode Condition.codec)) (StaticAbility.condition sa)
        <> Common.optionalPair "lingers" Nothing (Common.encodeMaybe (Codec.encode Duration.codec)) (StaticAbility.lingers sa)
        <> Common.requiredPair
          "modifications"
          (Common.encodeNonEmpty (Codec.encode Modification.codec))
          (StaticAbility.modifications sa)
    )

fromJson :: Value.Value -> Either Text.Text StaticAbility.StaticAbility
fromJson value = do
  ps <- Common.asObject value
  a <- Common.field "affected" ps >>= Codec.decode Affected.codec
  c <- Common.defaultedField "condition" Nothing (Common.decodeMaybe (Codec.decode Condition.codec)) ps
  l <- Common.defaultedField "lingers" Nothing (Common.decodeMaybe (Codec.decode Duration.codec)) ps
  ms <- Common.field "modifications" ps >>= Common.decodeNonEmpty (Codec.decode Modification.codec)
  pure (StaticAbility.MkStaticAbility a c l ms)

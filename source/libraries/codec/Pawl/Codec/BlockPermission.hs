module Pawl.Codec.BlockPermission where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.BlockPermission as BlockPermission

-- | An object with named keys, never a tagged sum: the type has one shape.
-- "additional" is REQUIRED rather than defaulted to one, because a permission
-- that adds nothing is not a thing any card prints and a missing key is far
-- likelier to be a typo than a deliberate zero. It is a whole Quantity, so the
-- common case reads {"type":"Literal","value":1} rather than a bare 1 -- the
-- price of Kemba's Legion's counted permission being the same field.
--
-- Which is also why "any number of creatures" is written as an explicit NULL
-- rather than by omitting the key: it is a positive statement the card makes,
-- and the required key keeps it distinguishable from a typo. "while" is the
-- ordinary optional field, omitted when the card states no gate, exactly as
-- Pawl.Codec.CombatRestriction spells "unless".
toJson :: BlockPermission.BlockPermission -> Value.Value
toJson bp =
  Value.object
    ( Common.requiredPair "affected" (Codec.encode Affected.codec) (BlockPermission.affected bp)
        <> Common.requiredPair "additional" (Common.encodeMaybe (Codec.encode Quantity.codec)) (BlockPermission.additional bp)
        <> Common.optionalPair "while" Nothing (Common.encodeMaybe (Codec.encode Condition.codec)) (BlockPermission.while bp)
    )

fromJson :: Value.Value -> Either Text.Text BlockPermission.BlockPermission
fromJson value = do
  ps <- Common.asObject value
  a <- Common.field "affected" ps >>= Codec.decode Affected.codec
  n <- Common.field "additional" ps >>= Common.decodeMaybe (Codec.decode Quantity.codec)
  c <- Common.defaultedField "while" Nothing (Common.decodeMaybe (Codec.decode Condition.codec)) ps
  pure (BlockPermission.MkBlockPermission a n c)

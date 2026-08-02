module Pawl.Codec.StaticAbility where

import qualified Data.Text as Text
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Modification as Modification
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.StaticAbility as StaticAbility

-- | CR 613.6: the parts of one ability's effect travel together, so the wire
-- format is one affected set and an ARRAY of modifications -- never one entry
-- per layer.
toJson :: StaticAbility.StaticAbility -> Value.Value
toJson sa =
  Common.object
    [ Common.pair "affected" . Affected.toJson $ StaticAbility.affected sa,
      Common.pair "modifications" . Common.encodeNonEmpty Modification.toJson $ StaticAbility.modifications sa
    ]

fromJson :: Value.Value -> Either Text.Text StaticAbility.StaticAbility
fromJson value = do
  ps <- Common.asObject value
  a <- Common.field "affected" ps >>= Affected.fromJson
  ms <- Common.field "modifications" ps >>= Common.decodeNonEmpty Modification.fromJson
  pure (StaticAbility.MkStaticAbility a ms)

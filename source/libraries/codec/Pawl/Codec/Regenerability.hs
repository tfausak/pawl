module Pawl.Codec.Regenerability where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Regenerability as Regenerability

toJson :: Regenerability.Regenerability -> Value.Value
toJson r = Common.nullary $ case r of
  Regenerability.Regenerable -> "Regenerable"
  Regenerability.CantBeRegenerated -> "CantBeRegenerated"

fromJson :: Value.Value -> Either Text.Text Regenerability.Regenerability
fromJson =
  Common.decodeNullary
    "Regenerability"
    [ ("Regenerable", Regenerability.Regenerable),
      ("CantBeRegenerated", Regenerability.CantBeRegenerated)
    ]

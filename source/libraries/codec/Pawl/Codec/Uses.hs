module Pawl.Codec.Uses where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Uses as Uses

toJson :: Uses.Uses -> Value.Value
toJson u = Common.nullary $ case u of
  Uses.Unlimited -> "Unlimited"
  Uses.Once -> "Once"

fromJson :: Value.Value -> Either Text.Text Uses.Uses
fromJson =
  Common.decodeNullary
    "Uses"
    [ ("Unlimited", Uses.Unlimited),
      ("Once", Uses.Once)
    ]

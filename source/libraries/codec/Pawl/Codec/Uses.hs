-- | The @Uses ⇆ Json@ codec (#481).
module Pawl.Codec.Uses where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Uses as Uses

usesToJson :: Uses.Uses -> Value
usesToJson u = Json.nullary . Text.pack $ case u of
  Uses.Unlimited -> "Unlimited"
  Uses.Once -> "Once"

jsonToUses :: Value -> Either Text Uses.Uses
jsonToUses =
  Json.decodeNullary
    (Text.pack "Uses")
    [ (Text.pack "Unlimited", Uses.Unlimited),
      (Text.pack "Once", Uses.Once)
    ]

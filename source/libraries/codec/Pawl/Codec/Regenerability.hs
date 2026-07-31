-- | The @Regenerability ⇆ Json@ codec (#481).
module Pawl.Codec.Regenerability where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Regenerability as Regenerability

regenerabilityToJson :: Regenerability.Regenerability -> Value
regenerabilityToJson r = Json.nullary . Text.pack $ case r of
  Regenerability.Regenerable -> "Regenerable"
  Regenerability.CantBeRegenerated -> "CantBeRegenerated"

jsonToRegenerability :: Value -> Either Text Regenerability.Regenerability
jsonToRegenerability =
  Json.decodeNullary
    (Text.pack "Regenerability")
    [ (Text.pack "Regenerable", Regenerability.Regenerable),
      (Text.pack "CantBeRegenerated", Regenerability.CantBeRegenerated)
    ]

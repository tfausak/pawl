module Pawl.JsonCodec.Codec where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Schema as Schema

-- | An encoder, a decoder and a schema as one value, so the three cannot drift
-- apart.
data Codec a = MkCodec
  { encode :: a -> Value.Value,
    decode :: Value.Value -> Either Text.Text a,
    schema :: Define.SchemaM Schema.Schema
  }

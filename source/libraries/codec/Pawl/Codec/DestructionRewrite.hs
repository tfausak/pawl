-- | The @DestructionRewrite ⇆ Json@ codec (#481).
module Pawl.Codec.DestructionRewrite where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite

destructionRewriteToJson :: DestructionRewrite.DestructionRewrite -> Value
destructionRewriteToJson r = Json.nullary . Text.pack $ case r of
  DestructionRewrite.Regenerate -> "Regenerate"

jsonToDestructionRewrite :: Value -> Either Text DestructionRewrite.DestructionRewrite
jsonToDestructionRewrite =
  Json.decodeNullary (Text.pack "DestructionRewrite") [(Text.pack "Regenerate", DestructionRewrite.Regenerate)]

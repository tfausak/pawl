-- | The @Supertype ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Supertype where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Supertype as Supertype

supertypeToJson :: Supertype.Supertype -> Value
supertypeToJson s = Json.nullary . Text.pack $ case s of
  Supertype.Basic -> "Basic"
  Supertype.Legendary -> "Legendary"
  Supertype.Snow -> "Snow"
  Supertype.World -> "World"

jsonToSupertype :: Value -> Either Text Supertype.Supertype
jsonToSupertype =
  Json.decodeNullary
    (Text.pack "Supertype")
    [ (Text.pack "Basic", Supertype.Basic),
      (Text.pack "Legendary", Supertype.Legendary),
      (Text.pack "Snow", Supertype.Snow),
      (Text.pack "World", Supertype.World)
    ]

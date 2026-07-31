-- | The @TapState ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.TapState where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.TapState as TapState

tapStateToJson :: TapState.TapState -> Value
tapStateToJson t = Json.nullary . Text.pack $ case t of
  TapState.Untapped -> "Untapped"
  TapState.Tapped -> "Tapped"

jsonToTapState :: Value -> Either Text TapState.TapState
jsonToTapState =
  Json.decodeNullary
    (Text.pack "TapState")
    [ (Text.pack "Untapped", TapState.Untapped),
      (Text.pack "Tapped", TapState.Tapped)
    ]

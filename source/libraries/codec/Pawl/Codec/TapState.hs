module Pawl.Codec.TapState where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.TapState as TapState

toJson :: TapState.TapState -> Value.Value
toJson t = Common.nullary $ case t of
  TapState.Untapped -> "Untapped"
  TapState.Tapped -> "Tapped"

fromJson :: Value.Value -> Either Text.Text TapState.TapState
fromJson =
  Common.decodeNullary
    "TapState"
    [ ("Untapped", TapState.Untapped),
      ("Tapped", TapState.Tapped)
    ]

module Pawl.Codec.Onset where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Onset as Onset

toJson :: Onset.Onset -> Value.Value
toJson o = Common.nullary $ case o of
  Onset.Immediately -> "Immediately"
  Onset.FromYourNextTurn -> "FromYourNextTurn"

fromJson :: Value.Value -> Either Text.Text Onset.Onset
fromJson =
  Common.decodeNullary
    "Onset"
    [ ("Immediately", Onset.Immediately),
      ("FromYourNextTurn", Onset.FromYourNextTurn)
    ]

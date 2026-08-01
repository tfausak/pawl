-- | The @Onset ⇆ Json@ codec (#481).
module Pawl.Codec.Onset where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Onset as Onset

onsetToJson :: Onset.Onset -> Value
onsetToJson o = case o of
  Onset.Immediately -> Json.nullary (Text.pack "Immediately")
  Onset.FromYourNextTurn -> Json.nullary (Text.pack "FromYourNextTurn")

jsonToOnset :: Value -> Either Text Onset.Onset
jsonToOnset =
  Json.decodeNullary
    (Text.pack "Onset")
    [ (Text.pack "Immediately", Onset.Immediately),
      (Text.pack "FromYourNextTurn", Onset.FromYourNextTurn)
    ]

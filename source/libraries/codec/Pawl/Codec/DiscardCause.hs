-- | The @DiscardCause ⇆ Json@ codec (#481).
module Pawl.Codec.DiscardCause where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.DiscardCause as DiscardCause

discardCauseToJson :: DiscardCause.DiscardCause -> Value
discardCauseToJson c = Json.nullary . Text.pack $ case c of
  DiscardCause.Ordinary -> "Ordinary"
  DiscardCause.ToPayCyclingCost -> "ToPayCyclingCost"

jsonToDiscardCause :: Value -> Either Text DiscardCause.DiscardCause
jsonToDiscardCause =
  Json.decodeNullary
    (Text.pack "DiscardCause")
    [ (Text.pack "Ordinary", DiscardCause.Ordinary),
      (Text.pack "ToPayCyclingCost", DiscardCause.ToPayCyclingCost)
    ]

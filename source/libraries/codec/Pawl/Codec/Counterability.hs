-- | The @Counterability ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Counterability where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value (Null))
import qualified Pawl.Types.Counterability as Counterability

counterabilityToJson :: Counterability.Counterability -> Value
counterabilityToJson c = Json.nullary . Text.pack $ case c of
  Counterability.Counterable -> "Counterable"
  Counterability.CantBeCountered -> "CantBeCountered"

-- Absent means Counterable (CR 113.6g is printed text: a card either says it or
-- does not), the shape Json.jsonToBoolDefault gives the other defaulted keys.
jsonToCounterabilityDefault :: Value -> Either Text Counterability.Counterability
jsonToCounterabilityDefault value = case value of
  Null _ -> Right Counterability.Counterable
  _ -> jsonToCounterability value

jsonToCounterability :: Value -> Either Text Counterability.Counterability
jsonToCounterability =
  Json.decodeNullary
    (Text.pack "Counterability")
    [ (Text.pack "Counterable", Counterability.Counterable),
      (Text.pack "CantBeCountered", Counterability.CantBeCountered)
    ]

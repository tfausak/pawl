-- | The @Loyalty ⇆ Json@ codec (#481).
module Pawl.Codec.Loyalty where

import Data.Text (Text)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Loyalty as Loyalty

loyaltyToJson :: Loyalty.Loyalty -> Value
loyaltyToJson = Json.natTo . Loyalty.unwrap

jsonToLoyalty :: Value -> Either Text Loyalty.Loyalty
jsonToLoyalty value = Loyalty.MkLoyalty <$> Json.natFrom value

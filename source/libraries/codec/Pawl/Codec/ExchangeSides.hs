module Pawl.Codec.ExchangeSides where

import qualified Data.Text as Text
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ExchangeSides as ExchangeSides

toJson :: ExchangeSides.ExchangeSides -> Value.Value
toJson sides = case sides of
  ExchangeSides.WithController n -> Common.tagged "WithController" . Just $ Codec.encode SlotName.codec n
  ExchangeSides.BetweenTargets n -> Common.tagged "BetweenTargets" . Just $ Codec.encode SlotName.codec n

fromJson :: Value.Value -> Either Text.Text ExchangeSides.ExchangeSides
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("WithController", Just v) -> ExchangeSides.WithController <$> Codec.decode SlotName.codec v
    ("BetweenTargets", Just v) -> ExchangeSides.BetweenTargets <$> Codec.decode SlotName.codec v
    _ -> Left . Text.pack $ "unknown ExchangeSides: " <> t

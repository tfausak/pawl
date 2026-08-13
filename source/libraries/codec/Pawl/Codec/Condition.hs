module Pawl.Codec.Condition where

import qualified Data.Text as Text
import qualified Pawl.Codec.Comparison as Comparison
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Condition as Condition

-- | Tagged like every other sum. The two shapes were previously told apart by
-- their KEYS -- a comparison by @measured@\/@comparison@\/@threshold@ and a
-- disjunction by a lone @any@ -- which read as well as a tag but could not be
-- stated as a schema a decoder guarantees (#1304).
--
-- Naming the comparison's sides survives the move: they are the tagged payload's
-- OWN keys, not the condition's, so a card file that swapped two Quantities is
-- still rejected rather than decoding into the wrong condition.
--
-- Still a loose toJson\/fromJson pair rather than an 'Pawl.JsonCodec.Arm.tagged'
-- bundle, but no longer for the reason it was: 'Pawl.Codec.Quantity' IS a bundle
-- now. What is left is @Compares@'s three-field payload, which has no Haskell
-- record for 'Pawl.JsonCodec.Fields.object' to name -- so this waits on the
-- payload records rather than on the conversion DAG (#1305).
toJson :: Condition.Condition -> Value.Value
toJson condition = case condition of
  Condition.Compares m c t ->
    Common.tagged "Compares" . Just . Value.object . concat $
      [ Common.requiredPair "measured" (Codec.encode Quantity.codec) m,
        Common.requiredPair "comparison" (Codec.encode Comparison.codec) c,
        Common.requiredPair "threshold" (Codec.encode Quantity.codec) t
      ]
  Condition.Any cs -> Common.tagged "Any" . Just $ Common.encodeList toJson cs

fromJson :: Value.Value -> Either Text.Text Condition.Condition
fromJson value = do
  (t, mv) <- Common.asTagged value
  case t of
    "Compares" -> Common.withValue mv $ \v -> do
      ps <- Common.asObject v
      Condition.Compares
        <$> (Common.field "measured" ps >>= Codec.decode Quantity.codec)
        <*> (Common.field "comparison" ps >>= Codec.decode Comparison.codec)
        <*> (Common.field "threshold" ps >>= Codec.decode Quantity.codec)
    "Any" -> Common.withValue mv $ fmap Condition.Any . Common.decodeList fromJson
    _ -> Left . Text.pack $ "unknown Condition: " <> t

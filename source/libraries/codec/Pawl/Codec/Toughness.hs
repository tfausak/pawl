module Pawl.Codec.Toughness where

import qualified Data.Text as Text
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Toughness as Toughness

toJson :: Toughness.Toughness -> Value.Value
toJson = Codec.encode Quantity.codec . Toughness.unwrap

fromJson :: Value.Value -> Either Text.Text Toughness.Toughness
fromJson = fmap Toughness.MkToughness . Codec.decode Quantity.codec

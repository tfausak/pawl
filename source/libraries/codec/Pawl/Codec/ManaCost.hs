module Pawl.Codec.ManaCost where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ManaSymbol as ManaSymbol
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ManaCost as ManaCost

toJson :: ManaCost.ManaCost -> Value.Value
toJson = Common.encodeList ManaSymbol.toJson . ManaCost.unwrap

fromJson :: Value.Value -> Either Text.Text ManaCost.ManaCost
fromJson = fmap ManaCost.MkManaCost . Common.decodeList ManaSymbol.fromJson

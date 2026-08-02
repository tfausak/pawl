module Pawl.Codec.CardName where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.CardName as CardName

fromJson :: Value.Value -> Either Text.Text CardName.CardName
fromJson = fmap CardName.MkCardName . Common.asText

toJson :: CardName.CardName -> Value.Value
toJson = Common.text . CardName.unwrap

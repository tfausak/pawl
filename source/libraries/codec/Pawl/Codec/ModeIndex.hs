module Pawl.Codec.ModeIndex where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ModeIndex as ModeIndex

toJson :: ModeIndex.ModeIndex -> Value.Value
toJson = Common.encodeNatural . ModeIndex.unwrap

fromJson :: Value.Value -> Either Text.Text ModeIndex.ModeIndex
fromJson = fmap ModeIndex.MkModeIndex . Common.decodeNatural

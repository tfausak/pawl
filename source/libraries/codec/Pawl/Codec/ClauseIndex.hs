module Pawl.Codec.ClauseIndex where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ClauseIndex as ClauseIndex

toJson :: ClauseIndex.ClauseIndex -> Value.Value
toJson = Common.encodeNatural . ClauseIndex.unwrap

fromJson :: Value.Value -> Either Text.Text ClauseIndex.ClauseIndex
fromJson = fmap ClauseIndex.MkClauseIndex . Common.decodeNatural

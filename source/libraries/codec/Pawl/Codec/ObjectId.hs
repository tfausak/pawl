module Pawl.Codec.ObjectId where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ObjectId as ObjectId

toJson :: ObjectId.ObjectId -> Value.Value
toJson = Common.encodeNatural . ObjectId.unwrap

fromJson :: Value.Value -> Either Text.Text ObjectId.ObjectId
fromJson = fmap ObjectId.MkObjectId . Common.decodeNatural

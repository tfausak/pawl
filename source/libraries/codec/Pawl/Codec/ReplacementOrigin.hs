module Pawl.Codec.ReplacementOrigin where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin

toJson :: ReplacementOrigin.ReplacementOrigin -> Value.Value
toJson o = Common.nullary $ case o of
  ReplacementOrigin.SelfReplacement -> "SelfReplacement"
  ReplacementOrigin.Other -> "Other"

fromJson :: Value.Value -> Either Text.Text ReplacementOrigin.ReplacementOrigin
fromJson =
  Common.decodeNullary
    "ReplacementOrigin"
    [ ("SelfReplacement", ReplacementOrigin.SelfReplacement),
      ("Other", ReplacementOrigin.Other)
    ]

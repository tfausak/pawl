module Pawl.Codec.SourceRelation where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SourceRelation as SourceRelation

toJson :: SourceRelation.SourceRelation -> Value.Value
toJson r = Common.nullary $ case r of
  SourceRelation.AnySource -> "AnySource"
  SourceRelation.TheSource -> "TheSource"

fromJson :: Value.Value -> Either Text.Text SourceRelation.SourceRelation
fromJson =
  Common.decodeNullary
    "SourceRelation"
    [ ("AnySource", SourceRelation.AnySource),
      ("TheSource", SourceRelation.TheSource)
    ]

module Pawl.Codec.Countering where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Countering as Countering

toJson :: Countering.Countering -> Value.Value
toJson c =
  Common.object . concat $
    [ Common.requiredPair "spell" ObjectId.toJson (Countering.spell c),
      Common.requiredPair "source" ObjectId.toJson (Countering.source c),
      Common.requiredPair "controller" PlayerId.toJson (Countering.controller c)
    ]

fromJson :: Value.Value -> Either Text.Text Countering.Countering
fromJson value = do
  ps <- Common.asObject value
  s <- Common.field "spell" ps >>= ObjectId.fromJson
  o <- Common.field "source" ps >>= ObjectId.fromJson
  c <- Common.field "controller" ps >>= PlayerId.fromJson
  pure
    Countering.MkCountering
      { Countering.spell = s,
        Countering.source = o,
        Countering.controller = c
      }

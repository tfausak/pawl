module Pawl.Codec.Mode where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.Optionality as Optionality
import qualified Pawl.Codec.TargetSpec as TargetSpec
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Optionality as Optionality

toJson :: (card -> Value.Value) -> Mode.Mode card -> Value.Value
toJson codec m =
  Common.object
    ( [ Common.pair "effects" (Common.encodeSeq (Effect.toJson codec) (Mode.effects m)),
        Common.pair "targetSpecs" (TargetSpec.toJsonMap (Mode.targetSpecs m))
      ]
        -- Omitted when Mandatory; see Optionality.fromJsonDefault.
        <> ( case Mode.optionality m of
               Optionality.Mandatory -> []
               Optionality.Optional -> [Common.pair "optionality" (Optionality.toJson (Mode.optionality m))]
           )
    )

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Mode.Mode card)
fromJson decode value = do
  ps <- Common.asObject value
  es <- Common.field "effects" ps >>= Common.decodeSeq (Effect.fromJson decode)
  ts <- Common.field "targetSpecs" ps >>= TargetSpec.fromJsonMap
  o <- Optionality.fromJsonDefault (Common.nullableField "optionality" ps)
  pure (Mode.MkMode es ts o)

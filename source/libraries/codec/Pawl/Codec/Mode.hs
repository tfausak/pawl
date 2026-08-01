-- | The @Mode ⇆ Json@ codec (#481).
module Pawl.Codec.Mode where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Effect (effectToJson, jsonToEffect)
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.Optionality as Optionality
import Pawl.Codec.TargetSpec (jsonToTargetSpecs, targetSpecsToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Optionality as Optionality

modeToJson :: (card -> Value) -> Mode.Mode card -> Value
modeToJson codec m =
  Json.jObject
    ( [ (Text.pack "effects", Json.seqTo (effectToJson codec) (Mode.effects m)),
        (Text.pack "targetSpecs", targetSpecsToJson (Mode.targetSpecs m))
      ]
        -- Omitted when Mandatory; see Optionality.fromJsonDefault.
        <> ( case Mode.optionality m of
               Optionality.Mandatory -> []
               Optionality.Optional -> [(Text.pack "optionality", Optionality.toJson (Mode.optionality m))]
           )
    )

jsonToMode :: (Value -> Either Text card) -> Value -> Either Text (Mode.Mode card)
jsonToMode decode value = do
  ps <- Json.asObject value
  es <- Json.field (Text.pack "effects") ps >>= Json.seqFrom (jsonToEffect decode)
  ts <- Json.field (Text.pack "targetSpecs") ps >>= jsonToTargetSpecs
  o <- Optionality.fromJsonDefault (Json.getOpt (Text.pack "optionality") ps)
  pure (Mode.MkMode es ts o)

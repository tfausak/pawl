-- | The @Modal ⇆ Json@ codec (#481).
module Pawl.Codec.Modal where

import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Mode (jsonToMode, modeToJson)
import Pawl.Codec.ModeSelection (jsonToModeSelection, modeSelectionToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Modal as Modal

modalToJson :: (card -> Value) -> Modal.Modal card -> Value
modalToJson codec m =
  Json.jObject
    [ (Text.pack "modes", Json.seqTo (modeToJson codec) (Modal.modes m)),
      (Text.pack "selection", modeSelectionToJson (Modal.selection m))
    ]

jsonToModal :: (Value -> Either Text card) -> Value -> Either Text (Modal.Modal card)
jsonToModal decode value = do
  ps <- Json.asObject value
  ms <- Json.field (Text.pack "modes") ps >>= Json.seqFrom (jsonToMode decode)
  if Seq.null ms
    then Left (Text.pack "modal has no modes")
    else do
      sel <- Json.field (Text.pack "selection") ps >>= jsonToModeSelection
      pure (Modal.MkModal ms sel)

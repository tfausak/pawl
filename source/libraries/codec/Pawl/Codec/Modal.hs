module Pawl.Codec.Modal where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Mode as Mode
import qualified Pawl.Codec.ModeSelection as ModeSelection
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Modal as Modal

toJson :: (card -> Value.Value) -> Modal.Modal card -> Value.Value
toJson codec m =
  Common.object
    [ Common.pair "modes" (Common.encodeSeq (Mode.toJson codec) (Modal.modes m)),
      Common.pair "selection" (ModeSelection.toJson (Modal.selection m))
    ]

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Modal.Modal card)
fromJson decode value = do
  ps <- Common.asObject value
  ms <- Common.field "modes" ps >>= Common.decodeSeq (Mode.fromJson decode)
  if Seq.null ms
    then Left (Text.pack "modal has no modes")
    else do
      sel <- Common.field "selection" ps >>= ModeSelection.fromJson
      pure (Modal.MkModal ms sel)

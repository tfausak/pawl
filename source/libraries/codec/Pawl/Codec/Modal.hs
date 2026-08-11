module Pawl.Codec.Modal where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Mode as Mode
import qualified Pawl.Codec.ModeSelection as ModeSelection
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.ModeSelection as ModeSelection

-- | CR 700.2 via Pawl.Types.Modal's header: "A non-modal payload is one Mode
-- with ChooseExactly 1", so this is what a card that says nothing about modes
-- means.
defaultSelection :: ModeSelection.ModeSelection
defaultSelection = ModeSelection.ChooseExactly 1

toJson :: (Eq card) => (card -> Value.Value) -> Modal.Modal card -> Value.Value
toJson codec m =
  Common.object
    ( Common.requiredPair "modes" (Common.encodeSeq (Mode.toJson codec)) (Modal.modes m)
        <> Common.optionalPair "selection" defaultSelection ModeSelection.toJson (Modal.selection m)
    )

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Modal.Modal card)
fromJson decode value = do
  ps <- Common.asObject value
  ms <- Common.field "modes" ps >>= Common.decodeSeq (Mode.fromJson decode)
  if Seq.null ms
    then Left (Text.pack "modal has no modes")
    else do
      sel <- Common.defaultedField "selection" defaultSelection ModeSelection.fromJson ps
      pure (Modal.MkModal ms sel)

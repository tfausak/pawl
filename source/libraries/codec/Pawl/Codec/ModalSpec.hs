{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ModalSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Codec.ModeSelection as ModeSelection
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSlot as TargetSlot

-- | The `card` parameter is instantiated at 'Text.Text' throughout.
-- 'Modal.codec' reach it only through the supplied Mode
-- codec, so any type proves the shape.
cardCodec :: Codec.Codec Text.Text
cardCodec = Common.text

codec :: Codec.Codec (Modal.Modal Text.Text)
codec = Modal.codec cardCodec

toJson :: Modal.Modal Text.Text -> Value.Value
toJson = Codec.encode codec

fromJson :: Value.Value -> Either Text.Text (Modal.Modal Text.Text)
fromJson = Codec.decode codec

-- One constructor, so three cases: a populated payload in CR 700.2's non-modal
-- shape, the `selection` field defaulted (the only omissible one -- `modes` is
-- required and non-empty by invariant), and the empty-modes decode failure.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Modal" $ do
  Spec.it s "MkModal, Bonesplitter's Equip payload" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Modal.MkModal
          ( Seq.singleton
              ( Mode.MkMode
                  (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Attach (SlotName.MkSlotName (Text.pack "target"))))))
                  (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSlot.required Pool.Creatures (Just (Filter.ControlledBy PlayerRelation.You))))
              )
          )
          (ModeSelection.ChooseExactly 1)
      )
      """ {"modes":[{"clauses":[{"effects":[{"type":"Attach","value":"target"}]}],"targetSlots":{"target":{"pool":{"type":"Creatures"},"filter":{"type":"ControlledBy","value":{"type":"You"}}}}}]} """
  Spec.it s "omits a ChooseExactly 1 selection" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) Modal.defaultSelection)
      """ {"modes":[{}]} """
  -- CR 700.2's non-modal payload is a single Mode: a modal PAYLOAD has at
  -- least one mode by invariant, so an empty `modes` array is a decode
  -- failure, not a spell that offers no choices.
  Spec.it s "empty modal is a decode error" $
    Spec.assertBool
      s
      ( either
          (const True)
          (const False)
          (fromJson (Value.object [Value.pair "modes" (Value.array []), Value.pair "selection" (Codec.encode ModeSelection.codec (ModeSelection.ChooseExactly 1))]))
      )
      "left"

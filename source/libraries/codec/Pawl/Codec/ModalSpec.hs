{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ModalSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Codec.ModeSelection as ModeSelection
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSpec as TargetSpec

-- | Every case below instantiates the `card` parameter at 'Text.Text', a
-- stand-in that is never a real card -- 'Modal.toJson'/'Modal.fromJson' reach
-- it only through the supplied Mode codec, exactly like
-- 'Pawl.Codec.EffectSpec's own cardToJson/cardFromJson.
cardToJson :: Text.Text -> Value.Value
cardToJson = Common.text

cardFromJson :: Value.Value -> Either Text.Text Text.Text
cardFromJson = Common.asText

toJson :: Modal.Modal Text.Text -> Value.Value
toJson = Modal.toJson cardToJson

fromJson :: Value.Value -> Either Text.Text (Modal.Modal Text.Text)
fromJson = Modal.fromJson cardFromJson

-- One constructor (MkModal), so three cases: a populated payload (Bonesplitter's
-- Equip, CR 702.6a/700.2's non-modal shape -- one Mode with no alternative),
-- every field defaulted at once, and the empty-modes decode failure moved from
-- Pawl.CodecSpec.
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
                  (Seq.singleton (Effect.Attach (SlotName.MkSlotName (Text.pack "target"))))
                  (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.ControlledBy PlayerRelation.You))))
                  Optionality.Mandatory
              )
          )
          (ModeSelection.ChooseExactly 1)
      )
      """ {"modes":[{"effects":[{"type":"Attach","value":"target"}],"targetSpecs":[{"slot":"target","spec":{"pool":{"type":"Creatures"},"filter":{"type":"ControlledBy","value":{"type":"You"}}}}]}]} """
  Spec.it s "omits a ChooseExactly 1 selection" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) Modal.defaultSelection)
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
          (fromJson (Common.object [Common.pair "modes" (Common.array []), Common.pair "selection" (ModeSelection.toJson (ModeSelection.ChooseExactly 1))]))
      )
      "left"

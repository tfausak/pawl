{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ModeSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Mode as Mode
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSpec as TargetSpec

-- | The `card` parameter is instantiated at 'Text.Text' throughout.
-- 'Mode.toJson'/'Mode.fromJson' reach it only through the supplied Effect
-- codec, so any type proves the shape.
cardToJson :: Text.Text -> Value.Value
cardToJson = Common.text

cardFromJson :: Value.Value -> Either Text.Text Text.Text
cardFromJson = Common.asText

toJson :: Mode.Mode Text.Text -> Value.Value
toJson = Mode.toJson cardToJson

fromJson :: Value.Value -> Either Text.Text (Mode.Mode Text.Text)
fromJson = Mode.fromJson cardFromJson

-- One constructor, so three cases: a populated mode, CR 603.5's `optionality`
-- flag when present, and every field defaulted at once.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Mode" $ do
  Spec.it s "MkMode, Bonesplitter's Equip payload" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Mode.MkMode
          (Seq.singleton (Effect.Attach (SlotName.MkSlotName (Text.pack "target"))))
          (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.ControlledBy PlayerRelation.You))))
          Optionality.Mandatory
      )
      """ {"effects":[{"type":"Attach","value":"target"}],"targetSpecs":[{"slot":"target","spec":{"pool":{"type":"Creatures"},"filter":{"type":"ControlledBy","value":{"type":"You"}}}}]} """
  -- CR 603.5: an Optional mode is what a printed "may" encodes to, and the key
  -- is emitted only for that value.
  Spec.it s "an Optional mode's optionality key is present" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Mode.MkMode Seq.empty Map.empty Optionality.Optional)
      """ {"optionality":{"type":"Optional"}} """
  -- A Mandatory mode with no effects or targetSpecs is what a card that says
  -- nothing extra means, and it round-trips through the empty object.
  Spec.it s "omits every default field" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)
      """ {} """

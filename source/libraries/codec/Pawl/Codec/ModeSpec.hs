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

-- | Every case below instantiates the `card` parameter at 'Text.Text', a
-- stand-in that is never a real card -- 'Mode.toJson'/'Mode.fromJson' reach it
-- only through the supplied Effect codec, exactly like
-- 'Pawl.Codec.EffectSpec's own cardToJson/cardFromJson.
cardToJson :: Text.Text -> Value.Value
cardToJson = Common.text

cardFromJson :: Value.Value -> Either Text.Text Text.Text
cardFromJson = Common.asText

toJson :: Mode.Mode Text.Text -> Value.Value
toJson = Mode.toJson cardToJson

fromJson :: Value.Value -> Either Text.Text (Mode.Mode Text.Text)
fromJson = Mode.fromJson cardFromJson

-- One constructor (MkMode), so the three cases below cover: a populated mode
-- (Bonesplitter's Equip payload, CR 702.6a), and the two elision directions of
-- CR 603.5's `optionality` flag, moved from Pawl.CodecSpec.
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
      "{\"effects\":[{\"type\":\"Attach\",\"value\":\"target\"}],\"targetSpecs\":[{\"slot\":\"target\",\"spec\":{\"pool\":{\"type\":\"Creatures\"},\"filter\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}}}]}"
  -- CR 603.5: an Optional mode is what a printed "may" encodes to, and the key
  -- is emitted only for that value.
  Spec.it s "an Optional mode's optionality key is present" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Mode.MkMode Seq.empty Map.empty Optionality.Optional)
      "{\"effects\":[],\"targetSpecs\":[],\"optionality\":{\"type\":\"Optional\"}}"
  -- The byte-identity guarantee for every card file that prints no "may": a
  -- Mandatory mode emits no key, and a mode with no key decodes back to
  -- Mandatory -- the Counterability precedent.
  Spec.it s "a Mandatory mode's optionality key is omitted, and an absent key decodes to Mandatory" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)
      "{\"effects\":[],\"targetSpecs\":[]}"

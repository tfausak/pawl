module Pawl.Codec.ModeSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Mode as Mode
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSlot as TargetSlot

-- | The `card` parameter is instantiated at 'Text.Text' throughout.
-- 'Mode.codec' reach it only through the supplied Effect
-- codec, so any type proves the shape.
cardCodec :: Codec.Codec Text.Text
cardCodec = Common.text

codec :: Codec.Codec (Mode.Mode Text.Text)
codec = Mode.codec cardCodec

toJson :: Mode.Mode Text.Text -> Value.Value
toJson = Codec.encode codec

fromJson :: Value.Value -> Either Text.Text (Mode.Mode Text.Text)
fromJson = Codec.decode codec

-- One constructor and two fields, so two cases: a populated mode, and the empty
-- one whose every key is omitted. The resolution-time riders belong to
-- Pawl.Types.Clause now (CR 608.2e), and Pawl.Codec.ClauseSpec covers them.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Mode" $ do
  Spec.it s "MkMode, Bonesplitter's Equip payload" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Mode.MkMode
          (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Attach (SlotName.MkSlotName (Text.pack "target"))))))
          (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSlot.required Pool.Creatures (Just (Filter.ControlledBy PlayerRelation.You))))
      )
      " {\"clauses\":[{\"effects\":[{\"type\":\"Attach\",\"value\":\"target\"}]}],\"targetSlots\":{\"target\":{\"pool\":{\"type\":\"Creatures\"},\"filter\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}}}} "
  -- A mode with no clauses or targetSlots is what a card that says nothing
  -- extra means, and it round-trips through the empty object.
  Spec.it s "omits every default field" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Mode.MkMode Seq.empty Map.empty)
      " {} "

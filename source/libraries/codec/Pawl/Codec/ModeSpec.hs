{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ModeSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Mode as Mode
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Clause as Clause
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
          (Seq.singleton (Clause.MkClause Optionality.Mandatory Nothing (Seq.singleton (Effect.Attach (SlotName.MkSlotName (Text.pack "target"))))))
          (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.ControlledBy PlayerRelation.You))))
      )
      """ {"clauses":[{"effects":[{"type":"Attach","value":"target"}]}],"targetSpecs":[{"slot":"target","spec":{"pool":{"type":"Creatures"},"filter":{"type":"ControlledBy","value":{"type":"You"}}}}]} """
  -- A mode with no clauses or targetSpecs is what a card that says nothing
  -- extra means, and it round-trips through the empty object.
  Spec.it s "omits every default field" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Mode.MkMode Seq.empty Map.empty)
      """ {} """

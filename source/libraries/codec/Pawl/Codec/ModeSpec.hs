{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ModeSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Mode as Mode
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.UnlessPaid as UnlessPaid

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

-- One constructor, so four cases: a populated mode, CR 603.5's `optionality`
-- flag when present, CR 118.12a's `unlessPaid` clause when present, and every
-- field defaulted at once.
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
          Nothing
      )
      """ {"effects":[{"type":"Attach","value":"target"}],"targetSpecs":[{"slot":"target","spec":{"pool":{"type":"Creatures"},"filter":{"type":"ControlledBy","value":{"type":"You"}}}}]} """
  -- CR 603.5: an Optional mode is what a printed "may" encodes to, and the key
  -- is emitted only for that value.
  Spec.it s "an Optional mode's optionality key is present" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Mode.MkMode Seq.empty Map.empty Optionality.Optional Nothing)
      """ {"optionality":{"type":"Optional"}} """
  -- CR 118.12a: Mana Leak's "unless its controller pays {3}" is what the
  -- unlessPaid key encodes, and it is emitted only when there is one.
  Spec.it s "a mode carrying an unless-paid cost writes the key" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Mode.MkMode
          (Seq.singleton (Effect.Counter (SlotName.MkSlotName (Text.pack "spell"))))
          (Map.singleton (SlotName.MkSlotName (Text.pack "spell")) (TargetSpec.MkTargetSpec Pool.Spells Nothing))
          Optionality.Mandatory
          ( Just
              UnlessPaid.MkUnlessPaid
                { UnlessPaid.payer = SlotName.MkSlotName (Text.pack "spell"),
                  UnlessPaid.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]), Cost.components = []}
                }
          )
      )
      """ {"effects":[{"type":"Counter","value":"spell"}],"targetSpecs":[{"slot":"spell","spec":{"pool":{"type":"Spells"}}}],"unlessPaid":{"payer":"spell","cost":{"mana":[{"type":"Generic","value":3}]}}} """
  -- A Mandatory mode with no effects or targetSpecs is what a card that says
  -- nothing extra means, and it round-trips through the empty object.
  Spec.it s "omits every default field" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory Nothing)
      """ {} """

module Pawl.Codec.Binding where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.ModeIndex as ModeIndex
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.SlotName as SlotName

-- Runtime-only, never in card JSON: the codec must stay total over the
-- transitive closure of what the game state carries.
toJson :: Binding.Binding -> Value.Value
toJson b =
  Value.object . concat $
    [ Common.optionalPair "targets" Nothing (Common.encodeMaybe (Common.encodeSet (Codec.encode Recipient.codec))) (Binding.targets b),
      Common.optionalPair "amount" Nothing (Common.encodeMaybe Common.encodeNatural) (Binding.amount b),
      Common.optionalPair "modes" Nothing (Common.encodeMaybe (Common.encodeSeq (Codec.encode ModeIndex.codec))) (Binding.modes b),
      Common.optionalPair "copy" Nothing (Common.encodeMaybe ProjectedCharacteristics.toJson) (Binding.copy b),
      Common.optionalPair "objects" Nothing (Common.encodeMaybe (Common.encodeSeq (Codec.encode ObjectId.codec))) (Binding.objects b)
    ]

fromJson :: Value.Value -> Either Text.Text Binding.Binding
fromJson value = do
  ps <- Common.asObject value
  t <- Common.defaultedField "targets" Nothing (Common.decodeMaybe (Common.decodeSet (Codec.decode Recipient.codec))) ps
  a <- Common.defaultedField "amount" Nothing (Common.decodeMaybe Common.decodeNatural) ps
  m <- Common.defaultedField "modes" Nothing (Common.decodeMaybe (Common.decodeSeq (Codec.decode ModeIndex.codec))) ps
  c <- Common.defaultedField "copy" Nothing (Common.decodeMaybe ProjectedCharacteristics.fromJson) ps
  o <- Common.defaultedField "objects" Nothing (Common.decodeMaybe (Common.decodeSeq (Codec.decode ObjectId.codec))) ps
  pure
    Binding.MkBinding
      { Binding.targets = t,
        Binding.amount = a,
        Binding.modes = m,
        Binding.copy = c,
        Binding.objects = o
      }

-- A name-keyed map as a JSON OBJECT keyed by the slot name.
toJsonMap :: Map.Map SlotName.SlotName Binding.Binding -> Value.Value
toJsonMap = Common.encodeTextMap SlotName.unwrap toJson

fromJsonMap :: Value.Value -> Either Text.Text (Map.Map SlotName.SlotName Binding.Binding)
fromJsonMap = Common.decodeTextMap SlotName.MkSlotName fromJson

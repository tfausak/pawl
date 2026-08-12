module Pawl.Codec.Binding where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.ModeIndex as ModeIndex
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.SlotName as SlotName
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
    [ Common.optionalPair "targets" Nothing (Common.encodeMaybe (Common.encodeSet Recipient.toJson)) (Binding.targets b),
      Common.optionalPair "amount" Nothing (Common.encodeMaybe Common.encodeNatural) (Binding.amount b),
      Common.optionalPair "modes" Nothing (Common.encodeMaybe (Common.encodeSeq (Codec.encode ModeIndex.codec))) (Binding.modes b),
      Common.optionalPair "copy" Nothing (Common.encodeMaybe ProjectedCharacteristics.toJson) (Binding.copy b),
      Common.optionalPair "objects" Nothing (Common.encodeMaybe (Common.encodeSeq (Codec.encode ObjectId.codec))) (Binding.objects b)
    ]

fromJson :: Value.Value -> Either Text.Text Binding.Binding
fromJson value = do
  ps <- Common.asObject value
  t <- Common.defaultedField "targets" Nothing (Common.decodeMaybe (Common.decodeSet Recipient.fromJson)) ps
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

-- A name-keyed map as a sorted array of entries, so the render is
-- deterministic.
toJsonMap :: Map.Map SlotName.SlotName Binding.Binding -> Value.Value
toJsonMap m =
  Common.encodeList
    (\(k, v) -> Value.object [Value.pair "slot" (Codec.encode SlotName.codec k), Value.pair "binding" (toJson v)])
    (Map.toAscList m)

fromJsonMap :: Value.Value -> Either Text.Text (Map.Map SlotName.SlotName Binding.Binding)
fromJsonMap value =
  let decodeEntry v = do
        ps <- Common.asObject v
        k <- Common.field "slot" ps >>= Codec.decode SlotName.codec
        b <- Common.field "binding" ps >>= fromJson
        pure (k, b)
   in Map.fromList <$> Common.decodeList decodeEntry value

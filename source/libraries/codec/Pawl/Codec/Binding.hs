module Pawl.Codec.Binding where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ModeIndex as ModeIndex
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.SlotName as SlotName

-- Runtime-only, never in card JSON: the codec must stay total over the
-- transitive closure of what the game state carries.
toJson :: Binding.Binding -> Value.Value
toJson b =
  Common.object . concat $
    [ Common.optionalPair "target" Nothing (Common.encodeMaybe Recipient.toJson) (Binding.target b),
      Common.optionalPair "amount" Nothing (Common.encodeMaybe Common.encodeNatural) (Binding.amount b),
      Common.optionalPair "modes" Nothing (Common.encodeMaybe (Common.encodeSet ModeIndex.toJson)) (Binding.modes b),
      Common.optionalPair "copy" Nothing (Common.encodeMaybe ProjectedCharacteristics.toJson) (Binding.copy b)
    ]

fromJson :: Value.Value -> Either Text.Text Binding.Binding
fromJson value = do
  ps <- Common.asObject value
  t <- Common.defaultedField "target" Nothing (Common.decodeMaybe Recipient.fromJson) ps
  a <- Common.defaultedField "amount" Nothing (Common.decodeMaybe Common.decodeNatural) ps
  m <- Common.defaultedField "modes" Nothing (Common.decodeMaybe (Common.decodeSet ModeIndex.fromJson)) ps
  c <- Common.defaultedField "copy" Nothing (Common.decodeMaybe ProjectedCharacteristics.fromJson) ps
  pure
    Binding.MkBinding
      { Binding.target = t,
        Binding.amount = a,
        Binding.modes = m,
        Binding.copy = c
      }

-- A name-keyed map as a sorted array of entries, so the render is
-- deterministic.
toJsonMap :: Map.Map SlotName.SlotName Binding.Binding -> Value.Value
toJsonMap m =
  Common.encodeList
    (\(k, v) -> Common.object [Common.pair "slot" (SlotName.toJson k), Common.pair "binding" (toJson v)])
    (Map.toAscList m)

fromJsonMap :: Value.Value -> Either Text.Text (Map.Map SlotName.SlotName Binding.Binding)
fromJsonMap value =
  let decodeEntry v = do
        ps <- Common.asObject v
        k <- Common.field "slot" ps >>= SlotName.fromJson
        b <- Common.field "binding" ps >>= fromJson
        pure (k, b)
   in Map.fromList <$> Common.decodeList decodeEntry value

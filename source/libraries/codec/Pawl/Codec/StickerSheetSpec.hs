module Pawl.Codec.StickerSheetSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.StickerSheet as StickerSheet
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilitySticker as AbilitySticker
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PowerToughnessSticker as PowerToughnessSticker
import qualified Pawl.Types.StickerSheet as StickerSheet

-- CR 123.2: Ancestral Hot Dog Minotaur, whose middle name sticker is two words.
minotaur :: StickerSheet.StickerSheet
minotaur =
  StickerSheet.MkStickerSheet
    { StickerSheet.number = 23,
      StickerSheet.name = Text.pack "Ancestral Hot Dog Minotaur",
      StickerSheet.names = Seq.fromList (fmap Text.pack ["Ancestral", "Hot Dog", "Minotaur"]),
      StickerSheet.art = 3,
      StickerSheet.abilities =
        Seq.fromList
          [ AbilitySticker.MkAbilitySticker {AbilitySticker.tickets = 2, AbilitySticker.keywords = Map.singleton (Keyword.Afflict 2) 1, AbilitySticker.abilities = []},
            AbilitySticker.MkAbilitySticker {AbilitySticker.tickets = 3, AbilitySticker.keywords = Map.singleton Keyword.Flying 1, AbilitySticker.abilities = []}
          ],
      StickerSheet.powerToughness =
        Seq.fromList
          [ PowerToughnessSticker.MkPowerToughnessSticker {PowerToughnessSticker.tickets = 2, PowerToughnessSticker.power = 1, PowerToughnessSticker.toughness = 4},
            PowerToughnessSticker.MkPowerToughnessSticker {PowerToughnessSticker.tickets = 5, PowerToughnessSticker.power = 8, PowerToughnessSticker.toughness = 6}
          ],
      StickerSheet.oracleText = Nothing
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.StickerSheet" $ do
  Spec.it s "decodes a sheet" $ do
    v <- Common.assertJson s " {\"number\":23,\"name\":\"Ancestral Hot Dog Minotaur\",\"names\":[\"Ancestral\",\"Hot Dog\",\"Minotaur\"],\"art\":3,\"abilities\":[{\"tickets\":2,\"keywords\":[{\"type\":\"Afflict\",\"value\":2}]},{\"tickets\":3,\"keywords\":[{\"type\":\"Flying\"}]}],\"powerToughness\":[{\"tickets\":2,\"power\":1,\"toughness\":4},{\"tickets\":5,\"power\":8,\"toughness\":6}]} "
    Spec.assertEq s (Codec.decode StickerSheet.codec v) (Right minotaur)
  Spec.it s "round trips" $
    Spec.assertEq s (Codec.decode StickerSheet.codec (Codec.encode StickerSheet.codec minotaur)) (Right minotaur)
  Spec.it s "has a schema" $ Common.assertHasSchema s StickerSheet.codec

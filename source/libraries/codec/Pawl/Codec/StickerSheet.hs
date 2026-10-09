{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.StickerSheet where

import qualified Pawl.Codec.AbilitySticker as AbilitySticker
import qualified Pawl.Codec.PowerToughnessSticker as PowerToughnessSticker
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.StickerSheet as StickerSheet

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec StickerSheet.StickerSheet
codec = Fields.object $ do
  number <- Fields.required "number" Common.natural StickerSheet.number
  name <- Fields.required "name" Common.text StickerSheet.name
  names <- Fields.required "names" (Common.seq Common.text) StickerSheet.names
  art <- Fields.required "art" Common.natural StickerSheet.art
  abilities <- Fields.required "abilities" (Common.seq AbilitySticker.codec) StickerSheet.abilities
  powerToughness <- Fields.required "powerToughness" (Common.seq PowerToughnessSticker.codec) StickerSheet.powerToughness
  oracleText <- Fields.defaulted "oracleText" Nothing (Common.maybe Common.text) StickerSheet.oracleText
  pure
    StickerSheet.MkStickerSheet
      { StickerSheet.number = number,
        StickerSheet.name = name,
        StickerSheet.names = names,
        StickerSheet.art = art,
        StickerSheet.abilities = abilities,
        StickerSheet.powerToughness = powerToughness,
        StickerSheet.oracleText = oracleText
      }

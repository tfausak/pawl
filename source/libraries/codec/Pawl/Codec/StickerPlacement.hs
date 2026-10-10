{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.StickerPlacement where

import qualified Pawl.Codec.StickerRef as StickerRef
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.StickerPlacement as StickerPlacement

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec StickerPlacement.StickerPlacement
codec = Fields.object $ do
  sticker <- Fields.required "sticker" StickerRef.codec StickerPlacement.sticker
  timestamp <- Fields.required "timestamp" Timestamp.codec StickerPlacement.timestamp
  position <- Fields.defaulted "position" Nothing (Common.maybe Common.natural) StickerPlacement.position
  pure
    StickerPlacement.MkStickerPlacement
      { StickerPlacement.sticker = sticker,
        StickerPlacement.timestamp = timestamp,
        StickerPlacement.position = position
      }

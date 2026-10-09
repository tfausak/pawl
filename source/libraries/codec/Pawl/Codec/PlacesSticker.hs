{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlacesSticker where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.StickerKind as StickerKind
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlacesSticker as PlacesSticker

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec PlacesSticker.PlacesSticker
codec = Fields.object $ do
  placer <- Fields.required "placer" PlayerRelation.codec PlacesSticker.placer
  kinds <- Fields.required "kinds" (Common.set StickerKind.codec) PlacesSticker.kinds
  pure
    PlacesSticker.MkPlacesSticker
      { PlacesSticker.placer = placer,
        PlacesSticker.kinds = kinds
      }

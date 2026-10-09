{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.StickerPut where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.StickerKind as StickerKind
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.StickerPut as StickerPut

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec StickerPut.StickerPut
codec = Fields.object $ do
  placer <- Fields.required "placer" PlayerId.codec StickerPut.placer
  object <- Fields.required "object" ObjectId.codec StickerPut.object
  kind <- Fields.required "kind" StickerKind.codec StickerPut.kind
  pure
    StickerPut.MkStickerPut
      { StickerPut.placer = placer,
        StickerPut.object = object,
        StickerPut.kind = kind
      }

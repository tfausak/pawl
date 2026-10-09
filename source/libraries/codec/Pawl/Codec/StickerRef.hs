{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.StickerRef where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.StickerKind as StickerKind
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.StickerRef as StickerRef

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec StickerRef.StickerRef
codec = Fields.object $ do
  owner <- Fields.required "owner" PlayerId.codec StickerRef.owner
  sheet <- Fields.required "sheet" Common.natural StickerRef.sheet
  kind <- Fields.required "kind" StickerKind.codec StickerRef.kind
  index <- Fields.required "index" Common.natural StickerRef.index
  pure
    StickerRef.MkStickerRef
      { StickerRef.owner = owner,
        StickerRef.sheet = sheet,
        StickerRef.kind = kind,
        StickerRef.index = index
      }

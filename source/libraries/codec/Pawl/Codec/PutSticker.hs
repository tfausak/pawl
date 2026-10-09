{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PutSticker where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.StickerKind as StickerKind
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PutSticker as PutSticker

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec PutSticker.PutSticker
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec PutSticker.player
  ref <- Fields.required "ref" ObjectRef.codec PutSticker.ref
  kinds <- Fields.required "kinds" (Common.set StickerKind.codec) PutSticker.kinds
  pure
    PutSticker.MkPutSticker
      { PutSticker.player = player,
        PutSticker.ref = ref,
        PutSticker.kinds = kinds
      }

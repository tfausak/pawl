{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PutSticker where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
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
  ticketCap <- Fields.defaulted "ticketCap" Nothing (Common.maybe Quantity.codec) PutSticker.ticketCap
  free <- Fields.defaulted "free" False Common.boolean PutSticker.free
  bound <- Fields.defaulted "bound" Nothing (Common.maybe SlotName.codec) PutSticker.bound
  pure
    PutSticker.MkPutSticker
      { PutSticker.player = player,
        PutSticker.ref = ref,
        PutSticker.kinds = kinds,
        PutSticker.ticketCap = ticketCap,
        PutSticker.free = free,
        PutSticker.bound = bound
      }

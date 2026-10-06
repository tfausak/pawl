{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ManifestedDread where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ManifestedDread as ManifestedDread

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec ManifestedDread.ManifestedDread
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec ManifestedDread.player
  cards <- Fields.required "cards" (Common.seq ObjectId.codec) ManifestedDread.cards
  pure
    ManifestedDread.MkManifestedDread
      { ManifestedDread.player = player,
        ManifestedDread.cards = cards
      }

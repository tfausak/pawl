{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Discarded where

import qualified Pawl.Codec.DiscardCause as DiscardCause
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Discarded as Discarded

-- | A bare object keyed by the record's field names, replacing the positional
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec Discarded.Discarded
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec Discarded.player
  card <- Fields.required "card" ObjectId.codec Discarded.card
  cause <- Fields.required "cause" DiscardCause.codec Discarded.cause
  pure
    Discarded.MkDiscarded
      { Discarded.player = player,
        Discarded.card = card,
        Discarded.cause = cause
      }

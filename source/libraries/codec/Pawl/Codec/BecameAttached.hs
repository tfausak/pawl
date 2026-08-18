{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BecameAttached where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BecameAttached as BecameAttached

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec BecameAttached.BecameAttached
codec = Fields.object $ do
  attachment <- Fields.required "attachment" ObjectId.codec BecameAttached.attachment
  host <- Fields.required "host" Recipient.codec BecameAttached.host
  pure
    BecameAttached.MkBecameAttached
      { BecameAttached.attachment = attachment,
        BecameAttached.host = host
      }

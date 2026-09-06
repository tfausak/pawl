{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BecameUnattached where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BecameUnattached as BecameUnattached

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec BecameUnattached.BecameUnattached
codec = Fields.object $ do
  attachment <- Fields.required "attachment" ObjectId.codec BecameUnattached.attachment
  host <- Fields.required "host" Recipient.codec BecameUnattached.host
  pure
    BecameUnattached.MkBecameUnattached
      { BecameUnattached.attachment = attachment,
        BecameUnattached.host = host
      }

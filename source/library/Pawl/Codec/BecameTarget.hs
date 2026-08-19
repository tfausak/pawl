{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BecameTarget where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.StackObjectKind as StackObjectKind
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BecameTarget as BecameTarget

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec BecameTarget.BecameTarget
codec = Fields.object $ do
  targeted <- Fields.required "targeted" Recipient.codec BecameTarget.targeted
  source <- Fields.required "source" ObjectId.codec BecameTarget.source
  kind <- Fields.required "kind" StackObjectKind.codec BecameTarget.kind
  controller <- Fields.required "controller" PlayerId.codec BecameTarget.controller
  pure
    BecameTarget.MkBecameTarget
      { BecameTarget.targeted = targeted,
        BecameTarget.source = source,
        BecameTarget.kind = kind,
        BecameTarget.controller = controller
      }

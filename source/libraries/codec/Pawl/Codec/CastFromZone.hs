{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CastFromZone where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.InZone as InZone
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CastFromZone as CastFromZone

-- | A bare object keyed by the record's field names. The zone reference is
-- Pawl.Codec.InZone's, so CR 400.1's shared/per-player invariant is enforced here
-- too: a permission naming "the battlefield's owner" never decodes.
codec :: Codec.Codec CastFromZone.CastFromZone
codec = Fields.object $ do
  from <- Fields.required "from" InZone.codec CastFromZone.from
  matching <- Fields.required "matching" (Filter.codec Keyword.codec) CastFromZone.matching
  pure CastFromZone.MkCastFromZone {CastFromZone.from = from, CastFromZone.matching = matching}

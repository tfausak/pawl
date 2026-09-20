{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CastFromZone where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.InZone as InZone
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PermissionLimit as PermissionLimit
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CastFromZone as CastFromZone
import qualified Pawl.Types.PermissionLimit as PermissionLimit

-- | A bare object keyed by the record's field names. The zone reference is
-- Pawl.Codec.InZone's, so CR 400.1's shared/per-player invariant is enforced here
-- too: a permission naming "the battlefield's owner" never decodes.
--
-- The limit DEFAULTS to Unlimited, which is the permission every card that
-- prints no budget grants (Future Sight, Garruk's Horde, Sen Triplets).
codec :: Codec.Codec CastFromZone.CastFromZone
codec = Fields.object $ do
  from <- Fields.required "from" InZone.codec CastFromZone.from
  matching <- Fields.required "matching" (Filter.codec Keyword.codec) CastFromZone.matching
  limit <- Fields.defaulted "limit" PermissionLimit.Unlimited PermissionLimit.codec CastFromZone.limit
  pure CastFromZone.MkCastFromZone {CastFromZone.from = from, CastFromZone.matching = matching, CastFromZone.limit = limit}

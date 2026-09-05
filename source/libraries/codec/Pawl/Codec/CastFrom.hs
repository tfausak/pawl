{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CastFrom where

import qualified Pawl.Codec.InZone as InZone
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CastFrom as CastFrom
import qualified Pawl.Types.PlayerRef as PlayerRef.Type
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | A bare object keyed by the record's field names, with the caster defaulted.
--
-- CR 109.5's "you" is the default because every printed clause of this family
-- either names it (Archfiend's Vessel, Breathless Knight) or is agentless, and
-- an agentless clause says @EachPlayer@ out loud rather than eliding it. The
-- ZONE is required: nothing about "was cast" implies a zone to have been cast
-- from.
codec :: Codec.Codec CastFrom.CastFrom
codec = Fields.object $ do
  caster <- Fields.defaulted "caster" (PlayerRef.Type.Relative PlayerRelation.You) PlayerRef.codec CastFrom.caster
  from <- Fields.required "from" InZone.codec CastFrom.from
  pure CastFrom.MkCastFrom {CastFrom.caster = caster, CastFrom.from = from}

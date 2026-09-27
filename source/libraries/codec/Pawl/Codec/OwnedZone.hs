{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.OwnedZone where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.OwnedZone as OwnedZone
import qualified Pawl.Types.PlayerRelation as PlayerRelation.Type

-- | A bare object keyed by the record's field names. The owner defaults to
-- AnyPlayer, the reading of a zone the text does not scope.
codec :: Codec.Codec OwnedZone.OwnedZone
codec = Fields.object $ do
  zone <- Fields.required "zone" Zone.codec OwnedZone.zone
  owner <- Fields.defaulted "owner" PlayerRelation.Type.AnyPlayer PlayerRelation.codec OwnedZone.owner
  pure OwnedZone.MkOwnedZone {OwnedZone.zone = zone, OwnedZone.owner = owner}

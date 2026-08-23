{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ContinuousEffect where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.GrantedAbility as GrantedAbility
import qualified Pawl.Codec.Modification as Modification
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect

-- | The card codec is a PARAMETER, the shape Pawl.Codec.Face and
-- Pawl.Codec.Halved take, because the type is parametric in the card for
-- Pawl.Types.StaticAbility's reason. The modification is instantiated at a
-- GRANTED ability, the wider of the two widths, since StaticAbility.lingers hands
-- a static ability's own modification over here.
codec ::
  (Typeable.Typeable card, Eq card) =>
  Codec.Codec card ->
  Codec.Codec (ContinuousEffect.ContinuousEffect card)
codec cardCodec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ContinuousEffect.source
  timestamp <- Fields.required "timestamp" Timestamp.codec ContinuousEffect.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ContinuousEffect.expiry
  modification <- Fields.required "modification" (Modification.codec (GrantedAbility.codec cardCodec)) ContinuousEffect.modification
  affected <- Fields.required "affected" Affected.codec ContinuousEffect.affected
  pure
    ContinuousEffect.MkContinuousEffect
      { ContinuousEffect.source = source,
        ContinuousEffect.timestamp = timestamp,
        ContinuousEffect.expiry = expiry,
        ContinuousEffect.modification = modification,
        ContinuousEffect.affected = affected
      }

{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PreventAllDamage where

import qualified Data.Sequence as Seq
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.DamageDirection as DamageDirection
import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DamageDirection as DamageDirection
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PreventNextDamage writes the countdown shield's.
--
-- All three optional keys are 'Fields.defaulted', which is what lets every
-- unbounded shield already in the pool round trip unchanged: a card naming no
-- kind, shielding a recipient and carrying no CR 615.5 clause writes exactly the
-- two keys Pawl.Codec.DurationRef used to.
--
-- The effect codec is a PARAMETER rather than an import, for the reason
-- Pawl.Types.PreventAllDamage gives: the record is parametric in the effect so
-- that neither module has to name the other.
codec ::
  (Typeable.Typeable effect, Eq effect) =>
  Codec.Codec effect ->
  Codec.Codec (PreventAllDamage.PreventAllDamage effect)
codec effectCodec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec PreventAllDamage.duration
  kind <- Fields.defaulted "kind" Nothing (Common.maybe DamageKind.codec) PreventAllDamage.kind
  ref <- Fields.required "ref" ObjectRef.codec PreventAllDamage.ref
  direction <- Fields.defaulted "direction" DamageDirection.DealtTo DamageDirection.codec PreventAllDamage.direction
  riders <- Fields.defaulted "riders" Seq.empty (Common.seq effectCodec) PreventAllDamage.riders
  pure
    PreventAllDamage.MkPreventAllDamage
      { PreventAllDamage.duration = duration,
        PreventAllDamage.kind = kind,
        PreventAllDamage.ref = ref,
        PreventAllDamage.direction = direction,
        PreventAllDamage.riders = riders
      }

{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PreventNextDamage where

import qualified Data.Sequence as Seq
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage

-- | A bare object keyed by the record's field names, replacing the
-- three-or-four-element array this arm used to write.
--
-- The effect codec is a PARAMETER rather than an import, for the reason
-- Pawl.Types.PreventNextDamage gives: the record is parametric in the effect so
-- that neither module has to name the other.
codec ::
  (Typeable.Typeable effect, Eq effect) =>
  Codec.Codec effect ->
  Codec.Codec (PreventNextDamage.PreventNextDamage effect)
codec effectCodec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec PreventNextDamage.duration
  kind <- Fields.defaulted "kind" Nothing (Common.maybe DamageKind.codec) PreventNextDamage.kind
  ref <- Fields.required "ref" ObjectRef.codec PreventNextDamage.ref
  quantity <- Fields.required "quantity" Quantity.codec PreventNextDamage.quantity
  riders <- Fields.defaulted "riders" Seq.empty (Common.seq effectCodec) PreventNextDamage.riders
  pure
    PreventNextDamage.MkPreventNextDamage
      { PreventNextDamage.duration = duration,
        PreventNextDamage.kind = kind,
        PreventNextDamage.ref = ref,
        PreventNextDamage.quantity = quantity,
        PreventNextDamage.riders = riders
      }

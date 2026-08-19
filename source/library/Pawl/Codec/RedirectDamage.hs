{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RedirectDamage where

import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.RedirectDamage as RedirectDamage

-- | A bare object keyed by the record's field names. Naming them is the point:
-- 'RedirectDamage.from' and 'RedirectDamage.to' are both an ObjectRef, so a
-- positional payload let a card file redirect the damage backwards and still
-- decode.
codec :: Codec.Codec RedirectDamage.RedirectDamage
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec RedirectDamage.duration
  kind <- Fields.defaulted "kind" Nothing (Common.maybe DamageKind.codec) RedirectDamage.kind
  from <- Fields.required "from" ObjectRef.codec RedirectDamage.from
  to <- Fields.required "to" ObjectRef.codec RedirectDamage.to
  pure
    RedirectDamage.MkRedirectDamage
      { RedirectDamage.duration = duration,
        RedirectDamage.kind = kind,
        RedirectDamage.from = from,
        RedirectDamage.to = to
      }

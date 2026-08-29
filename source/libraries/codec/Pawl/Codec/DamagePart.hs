{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DamagePart where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DamagePart as DamagePart

-- | A bare object keyed by the record's field names, written inside
-- Pawl.Codec.DealDamage's "parts" array.
codec :: Codec.Codec DamagePart.DamagePart
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec DamagePart.ref
  quantity <- Fields.required "quantity" Quantity.codec DamagePart.quantity
  pure DamagePart.MkDamagePart {DamagePart.ref = ref, DamagePart.quantity = quantity}

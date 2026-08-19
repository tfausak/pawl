{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DealDamage where

import qualified Pawl.Codec.ExcessDestination as ExcessDestination
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DealDamage as DealDamage

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's DealDamage arm.
codec :: Codec.Codec DealDamage.DealDamage
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec DealDamage.ref
  quantity <- Fields.required "quantity" Quantity.codec DealDamage.quantity
  dealer <- Fields.defaulted "dealer" Nothing (Common.maybe SlotName.codec) DealDamage.dealer
  -- Defaulted to Nothing, so every card that deals damage without saying where
  -- the excess goes -- which is every one of them but Flame Spill -- keeps
  -- parsing unchanged.
  excess <- Fields.defaulted "excess" Nothing (Common.maybe ExcessDestination.codec) DealDamage.excess
  pure
    DealDamage.MkDealDamage
      { DealDamage.ref = ref,
        DealDamage.quantity = quantity,
        DealDamage.dealer = dealer,
        DealDamage.excess = excess
      }

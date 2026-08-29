{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DealDamage where

import qualified Pawl.Codec.DamagePart as DamagePart
import qualified Pawl.Codec.ExcessDestination as ExcessDestination
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DealDamage as DealDamage

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's DealDamage arm.
codec :: Codec.Codec DealDamage.DealDamage
codec = Fields.object $ do
  parts <- Fields.required "parts" (Common.seq DamagePart.codec) DealDamage.parts
  dealer <- Fields.defaulted "dealer" Nothing (Common.maybe SlotName.codec) DealDamage.dealer
  -- Defaulted to Nothing, so every card that deals damage without saying where
  -- the excess goes -- which is every one of them but Flame Spill -- keeps
  -- parsing unchanged.
  excess <- Fields.defaulted "excess" Nothing (Common.maybe ExcessDestination.codec) DealDamage.excess
  pure
    DealDamage.MkDealDamage
      { DealDamage.parts = parts,
        DealDamage.dealer = dealer,
        DealDamage.excess = excess
      }

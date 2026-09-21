{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CostBasis where

import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CostBasis as CostBasis
import qualified Pawl.Types.ManaCost as ManaCost.Type

-- | A bare object keyed by the record's field names.
--
-- The slot is REQUIRED -- a basis naming no object describes no cost. The
-- reduction is elided when there is none, the empty ManaCost being {0} (CR
-- 118.5); Flash is the one card in `data/cards/` that writes it.
codec :: Codec.Codec CostBasis.CostBasis
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec CostBasis.slot
  reducedBy <- Fields.defaulted "reducedBy" (ManaCost.Type.MkManaCost []) ManaCost.codec CostBasis.reducedBy
  pure
    CostBasis.MkCostBasis
      { CostBasis.slot = slot,
        CostBasis.reducedBy = reducedBy
      }

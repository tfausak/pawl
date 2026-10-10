{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CostAddition where

import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.CostScale as CostScale
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CostAddition as CostAddition
import qualified Pawl.Types.CostScale as CostScale.Type

-- | A bare object keyed by the record's field names. @scale@ is DEFAULTED to
-- Once: a sentence with no "for each" in it (Brutal Suppression) adds its
-- components once, so only Drought's card file writes the key.
codec :: Codec.Codec CostAddition.CostAddition
codec = Fields.object $ do
  components <- Fields.required "components" (Common.list (CostComponent.codec Keyword.codec)) CostAddition.components
  scale <- Fields.defaulted "scale" CostScale.Type.Once CostScale.codec CostAddition.scale
  pure CostAddition.MkCostAddition {CostAddition.components = components, CostAddition.scale = scale}

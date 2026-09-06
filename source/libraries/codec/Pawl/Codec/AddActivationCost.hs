{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AddActivationCost where

import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.CostScale as CostScale
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.LoyaltyKind as LoyaltyKind
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AddActivationCost as AddActivationCost
import qualified Pawl.Types.CostScale as CostScale

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
--
-- @scale@ is DEFAULTED: a sentence with no "for each" in it (Brutal Suppression,
-- Carth the Lion) adds its components once, so the key is written only by a
-- sentence that scales and no existing card file changes.
--
-- @whichLoyalty@ is DEFAULTED to Nothing for Pawl.Codec.IncreaseActivationCost's
-- @whichKind@ reason: Nothing is "every activated ability of a matching
-- source", which is what Brutal Suppression and Drought print, so only Carth the
-- Lion's card file writes the key.
codec :: Codec.Codec AddActivationCost.AddActivationCost
codec = Fields.object $ do
  whichAbilities <- Fields.required "whichAbilities" (Filter.codec Keyword.codec) AddActivationCost.whichAbilities
  whichLoyalty <- Fields.defaulted "whichLoyalty" Nothing (Common.maybe LoyaltyKind.codec) AddActivationCost.whichLoyalty
  components <- Fields.required "components" (Common.list (CostComponent.codec Keyword.codec)) AddActivationCost.components
  scale <- Fields.defaulted "scale" CostScale.Once CostScale.codec AddActivationCost.scale
  pure
    AddActivationCost.MkAddActivationCost
      { AddActivationCost.whichAbilities = whichAbilities,
        AddActivationCost.whichLoyalty = whichLoyalty,
        AddActivationCost.components = components,
        AddActivationCost.scale = scale
      }

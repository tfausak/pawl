{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AddActivationCost where

import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AddActivationCost as AddActivationCost

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec AddActivationCost.AddActivationCost
codec = Fields.object $ do
  whichAbilities <- Fields.required "whichAbilities" (Filter.codec Keyword.codec) AddActivationCost.whichAbilities
  components <- Fields.required "components" (Common.list (CostComponent.codec Keyword.codec)) AddActivationCost.components
  pure
    AddActivationCost.MkAddActivationCost
      { AddActivationCost.whichAbilities = whichAbilities,
        AddActivationCost.components = components
      }

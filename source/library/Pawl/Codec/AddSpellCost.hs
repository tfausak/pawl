{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AddSpellCost where

import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.CostScale as CostScale
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AddSpellCost as AddSpellCost
import qualified Pawl.Types.CostScale as CostScale

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.AddActivationCost is -- and @scale@ is defaulted there too, for the
-- same reason.
codec :: Codec.Codec AddSpellCost.AddSpellCost
codec = Fields.object $ do
  whichSpells <- Fields.required "whichSpells" (Filter.codec Keyword.codec) AddSpellCost.whichSpells
  components <- Fields.required "components" (Common.list (CostComponent.codec Keyword.codec)) AddSpellCost.components
  scale <- Fields.defaulted "scale" CostScale.Once CostScale.codec AddSpellCost.scale
  pure
    AddSpellCost.MkAddSpellCost
      { AddSpellCost.whichSpells = whichSpells,
        AddSpellCost.components = components,
        AddSpellCost.scale = scale
      }

module Pawl.Codec.AddActivationCostSpec where

import qualified Pawl.Codec.AddActivationCost as AddActivationCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AddActivationCost as AddActivationCost
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostScale as CostScale
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AddActivationCost" $ do
  -- CR 601.2f, as Brutal Suppression taxes a nontoken Rebel's ability.
  Spec.it s "MkAddActivationCost" $
    Common.assertCodec
      s
      AddActivationCost.codec
      ( AddActivationCost.MkAddActivationCost
          { AddActivationCost.whichAbilities = Filter.And [Filter.HasSubtype Subtype.Rebel, Filter.Not Filter.IsToken],
            AddActivationCost.components = [CostComponent.Sacrifice (Sacrifice.MkSacrifice 1 (Filter.HasCardType CardType.Land))],
            AddActivationCost.scale = CostScale.Once
          }
      )
      " {\"whichAbilities\":{\"type\":\"And\",\"value\":[{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Rebel\"}},{\"type\":\"Not\",\"value\":{\"type\":\"IsToken\"}}]},\"components\":[{\"type\":\"Sacrifice\",\"value\":{\"count\":1,\"whichPermanents\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}}]} "
  -- The other half of the defaulted key: Drought's activation sentence writes
  -- one, so the key appears on the wire.
  Spec.it s "MkAddActivationCost, a scale written on the wire" $
    Common.assertCodec
      s
      AddActivationCost.codec
      ( AddActivationCost.MkAddActivationCost
          { AddActivationCost.whichAbilities = Filter.And [],
            AddActivationCost.components = [CostComponent.Sacrifice (Sacrifice.MkSacrifice 1 (Filter.HasSubtype Subtype.Swamp))],
            AddActivationCost.scale = CostScale.PerColoredSymbol Color.Black
          }
      )
      " {\"whichAbilities\":{\"type\":\"And\",\"value\":[]},\"components\":[{\"type\":\"Sacrifice\",\"value\":{\"count\":1,\"whichPermanents\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Swamp\"}}}}],\"scale\":{\"type\":\"PerColoredSymbol\",\"value\":{\"type\":\"Black\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AddActivationCost.codec

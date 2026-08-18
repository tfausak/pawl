module Pawl.Codec.AddSpellCostSpec where

import qualified Pawl.Codec.AddSpellCost as AddSpellCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AddSpellCost as AddSpellCost
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostScale as CostScale
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AddSpellCost" $ do
  -- CR 601.2f as Drought writes it: every spell, "Sacrifice a Swamp", once per
  -- black mana symbol.
  Spec.it s "MkAddSpellCost" $
    Common.assertCodec
      s
      AddSpellCost.codec
      ( AddSpellCost.MkAddSpellCost
          { AddSpellCost.whichSpells = Filter.And [],
            AddSpellCost.components = [CostComponent.Sacrifice (Sacrifice.MkSacrifice 1 (Filter.HasSubtype Subtype.Swamp))],
            AddSpellCost.scale = CostScale.PerColoredSymbol Color.Black
          }
      )
      " {\"whichSpells\":{\"type\":\"And\",\"value\":[]},\"components\":[{\"type\":\"Sacrifice\",\"value\":{\"count\":1,\"whichPermanents\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Swamp\"}}}}],\"scale\":{\"type\":\"PerColoredSymbol\",\"value\":{\"type\":\"Black\"}}} "
  -- The pair the defaulted key needs: an absent @scale@ IS Once, and encoding
  -- Once writes no key.
  Spec.it s "MkAddSpellCost, an absent scale is Once" $
    Common.assertCodec
      s
      AddSpellCost.codec
      ( AddSpellCost.MkAddSpellCost
          { AddSpellCost.whichSpells = Filter.And [],
            AddSpellCost.components = [CostComponent.PayLife 2],
            AddSpellCost.scale = CostScale.Once
          }
      )
      " {\"whichSpells\":{\"type\":\"And\",\"value\":[]},\"components\":[{\"type\":\"PayLife\",\"value\":2}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AddSpellCost.codec

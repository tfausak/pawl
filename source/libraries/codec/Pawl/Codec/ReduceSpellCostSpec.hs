module Pawl.Codec.ReduceSpellCostSpec where

import qualified Pawl.Codec.ReduceSpellCost as ReduceSpellCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ReduceSpellCost as ReduceSpellCost

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ReduceSpellCost" $ do
  -- CR 118.7, as Sapphire Medallion reduces a blue spell by {1}.
  Spec.it s "MkReduceSpellCost" $
    Common.assertCodec
      s
      ReduceSpellCost.codec
      ( ReduceSpellCost.MkReduceSpellCost
          { ReduceSpellCost.whichSpells = Filter.HasColor Color.Blue,
            ReduceSpellCost.reduction = ManaCost.MkManaCost [ManaSymbol.Generic 1],
            ReduceSpellCost.coloredOnly = False,
            ReduceSpellCost.perTarget = Nothing
          }
      )
      " {\"whichSpells\":{\"type\":\"HasColor\",\"value\":{\"type\":\"Blue\"}},\"reduction\":[{\"type\":\"Generic\",\"value\":1}]} "
  -- CR 101.1, as Edgewalker prints "This effect reduces only the amount of
  -- colored mana you pay": the one shape that writes the defaulted key.
  Spec.it s "MkReduceSpellCost coloredOnly" $
    Common.assertCodec
      s
      ReduceSpellCost.codec
      ( ReduceSpellCost.MkReduceSpellCost
          { ReduceSpellCost.whichSpells = Filter.HasColor Color.Blue,
            ReduceSpellCost.reduction = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Blue)],
            ReduceSpellCost.coloredOnly = True,
            ReduceSpellCost.perTarget = Nothing
          }
      )
      " {\"whichSpells\":{\"type\":\"HasColor\",\"value\":{\"type\":\"Blue\"}},\"reduction\":[{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Blue\"}}}],\"coloredOnly\":true} "
  -- CR 601.2c / 601.2f, as Battlefield Thaumaturge reduces a spell {1} for
  -- each creature it targets.
  Spec.it s "MkReduceSpellCost perTarget" $
    Common.assertCodec
      s
      ReduceSpellCost.codec
      ( ReduceSpellCost.MkReduceSpellCost
          { ReduceSpellCost.whichSpells = Filter.HasCardType CardType.Instant,
            ReduceSpellCost.reduction = ManaCost.MkManaCost [ManaSymbol.Generic 1],
            ReduceSpellCost.coloredOnly = False,
            ReduceSpellCost.perTarget = Just (Filter.HasCardType CardType.Creature)
          }
      )
      " {\"whichSpells\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Instant\"}},\"reduction\":[{\"type\":\"Generic\",\"value\":1}],\"perTarget\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ReduceSpellCost.codec

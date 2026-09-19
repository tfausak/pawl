module Pawl.Codec.CostChoiceSpec where

import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Pawl.Codec.CostChoice as CostChoice
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostChoice as CostChoice
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostChoice" $ do
  -- Caustic Exhale's "behold a Dragon or pay {1}", one option of each half: a
  -- component and no mana, then mana and no component.
  Spec.it s "MkCostChoice" $
    Common.assertCodec
      s
      CostChoice.codec
      ( CostChoice.MkCostChoice
          ( Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost []), Cost.components = [CostComponent.Behold (Filter.HasCardType CardType.Creature)]}
              NonEmpty.:| [Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]), Cost.components = []}]
          )
      )
      " [{\"components\":[{\"type\":\"Behold\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}}],\"mana\":[]},{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}] "
  -- CR 118.6 by accident: a choice with no options has no way to be paid, which
  -- Common.nonEmpty is what refuses.
  Spec.it s "an empty array is rejected rather than decoded" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " [] ") >>= Codec.decode CostChoice.codec))
      "expected an empty options array to fail to decode"
  Spec.it s "has a schema" $
    Common.assertHasSchema s CostChoice.codec

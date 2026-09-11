module Pawl.Codec.AbilityAddsManaSpec where

import qualified Pawl.Codec.AbilityAddsMana as AbilityAddsMana
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityAddsMana as AbilityAddsMana
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaSpecification as ManaSpecification
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AbilityAddsMana" $ do
  -- Caged Sun's payload: you, a land's ability, mana of the chosen color.
  Spec.it s "MkAbilityAddsMana, every key" $
    Common.assertCodec
      s
      AbilityAddsMana.codec
      ( AbilityAddsMana.MkAbilityAddsMana
          { AbilityAddsMana.player = PlayerRelation.You,
            AbilityAddsMana.source = Filter.HasCardType CardType.Land,
            AbilityAddsMana.mana = ManaSpecification.ChosenColor
          }
      )
      " {\"player\":{\"type\":\"You\"},\"source\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"mana\":{\"type\":\"ChosenColor\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AbilityAddsMana.codec

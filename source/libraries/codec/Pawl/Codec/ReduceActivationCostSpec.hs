module Pawl.Codec.ReduceActivationCostSpec where

import qualified Pawl.Codec.ReduceActivationCost as ReduceActivationCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ReduceActivationCost as ReduceActivationCost

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ReduceActivationCost" $ do
  -- CR 101.1: the floor is card text, not a rule. Heartstone says "can't reduce
  -- ... to less than one mana" and carries 1; Blossoming Tortoise says nothing
  -- and carries 0. Both are written, since neither is a default.
  Spec.it s "MkReduceActivationCost, a stated floor" $
    Common.assertCodec
      s
      ReduceActivationCost.codec
      ( ReduceActivationCost.MkReduceActivationCost
          { ReduceActivationCost.whichAbilities = Filter.HasCardType CardType.Creature,
            ReduceActivationCost.reduction = ManaCost.MkManaCost [ManaSymbol.Generic 1],
            ReduceActivationCost.floor = 1
          }
      )
      " {\"whichAbilities\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"reduction\":[{\"type\":\"Generic\",\"value\":1}],\"floor\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ReduceActivationCost.codec

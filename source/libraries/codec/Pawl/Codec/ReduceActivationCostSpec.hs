module Pawl.Codec.ReduceActivationCostSpec where

import qualified Pawl.Codec.ReduceActivationCost as ReduceActivationCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.KeywordFamily as KeywordFamily
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
            ReduceActivationCost.grantedBy = Nothing,
            ReduceActivationCost.whichTargets = Nothing,
            ReduceActivationCost.reduction = ManaCost.MkManaCost [ManaSymbol.Generic 1],
            ReduceActivationCost.floor = 1
          }
      )
      " {\"whichAbilities\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"reduction\":[{\"type\":\"Generic\",\"value\":1}],\"floor\":1} "
  -- CR 702.29a: Fluctuator's "cycling abilities", the shape whose grantedBy is
  -- written. Its own case rather than an edit to the one above, because
  -- grantedBy is DEFAULTED -- a round trip whose value is Nothing encodes no key
  -- at all and would stay green with the field dropped from the codec (#1715's
  -- shape, arrived at through a default rather than an Arm.tagged wildcard).
  Spec.it s "MkReduceActivationCost, narrowed to one keyword family" $
    Common.assertCodec
      s
      ReduceActivationCost.codec
      ( ReduceActivationCost.MkReduceActivationCost
          { ReduceActivationCost.whichAbilities = Filter.And [],
            ReduceActivationCost.grantedBy = Just KeywordFamily.Cycling,
            ReduceActivationCost.whichTargets = Nothing,
            ReduceActivationCost.reduction = ManaCost.MkManaCost [ManaSymbol.Generic 2],
            ReduceActivationCost.floor = 0
          }
      )
      " {\"whichAbilities\":{\"type\":\"And\",\"value\":[]},\"grantedBy\":{\"type\":\"Cycling\"},\"reduction\":[{\"type\":\"Generic\",\"value\":2}],\"floor\":0} "
  -- CR 601.2c from the reducer's side: Dwarven Mauler's "equip abilities you
  -- activate THAT TARGET THIS CREATURE", the one shape in the pool that writes
  -- whichTargets. Its own case for the reason the grantedBy case above has one --
  -- the field is DEFAULTED, so a round trip whose value is Nothing would stay
  -- green with it dropped from the codec.
  Spec.it s "MkReduceActivationCost, narrowed to what the ability targets" $
    Common.assertCodec
      s
      ReduceActivationCost.codec
      ( ReduceActivationCost.MkReduceActivationCost
          { ReduceActivationCost.whichAbilities = Filter.And [],
            ReduceActivationCost.grantedBy = Just KeywordFamily.Equip,
            ReduceActivationCost.whichTargets = Just Filter.IsSource,
            ReduceActivationCost.reduction = ManaCost.MkManaCost [ManaSymbol.Generic 2],
            ReduceActivationCost.floor = 0
          }
      )
      " {\"whichAbilities\":{\"type\":\"And\",\"value\":[]},\"grantedBy\":{\"type\":\"Equip\"},\"whichTargets\":{\"type\":\"IsSource\"},\"reduction\":[{\"type\":\"Generic\",\"value\":2}],\"floor\":0} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ReduceActivationCost.codec

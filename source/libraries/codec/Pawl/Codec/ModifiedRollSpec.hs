module Pawl.Codec.ModifiedRollSpec where

import qualified Pawl.Codec.ModifiedRoll as ModifiedRoll
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ModifiedRoll as ModifiedRoll
import qualified Pawl.Types.PermissionLimit as PermissionLimit
import qualified Pawl.Types.RollModifier as RollModifier
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapPermanents as TapPermanents

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ModifiedRoll" $ do
  -- CR 706.2 as Clam-I-Am prints it, and the wire form
  -- data/cards/clam-i-am.json writes: a six-sided die that came up a 3.
  Spec.it s "MkModifiedRoll, Clam-I-Am's narrowed reroll" $
    Common.assertCodec
      s
      ModifiedRoll.codec
      ModifiedRoll.MkModifiedRoll
        { ModifiedRoll.sides = Just 6,
          ModifiedRoll.natural = Just 3,
          ModifiedRoll.modifier = RollModifier.Reroll,
          ModifiedRoll.cost = Nothing,
          ModifiedRoll.limit = PermissionLimit.Unlimited
        }
      " {\"sides\":6,\"natural\":3,\"modifier\":{\"type\":\"Reroll\"}} "
  -- Both narrowings and CR 706.2a's cost default away, which is Wall of
  -- Fortune's bare "a die" with its cost dropped.
  Spec.it s "MkModifiedRoll narrowing nothing" $
    Common.assertCodec
      s
      ModifiedRoll.codec
      ModifiedRoll.MkModifiedRoll
        { ModifiedRoll.sides = Nothing,
          ModifiedRoll.natural = Nothing,
          ModifiedRoll.modifier = RollModifier.Reroll,
          ModifiedRoll.cost = Nothing,
          ModifiedRoll.limit = PermissionLimit.Unlimited
        }
      " {\"modifier\":{\"type\":\"Reroll\"}} "
  -- CR 706.2a's associated cost, and the wire form
  -- data/cards/wall-of-fortune.json writes: a non-mana cost of one tap.
  Spec.it s "MkModifiedRoll, Wall of Fortune's costed reroll" $
    Common.assertCodec
      s
      ModifiedRoll.codec
      ModifiedRoll.MkModifiedRoll
        { ModifiedRoll.sides = Nothing,
          ModifiedRoll.natural = Nothing,
          ModifiedRoll.modifier = RollModifier.Reroll,
          ModifiedRoll.cost =
            Just
              Cost.MkCost
                { Cost.mana = Just (ManaCost.MkManaCost []),
                  Cost.components =
                    [ CostComponent.TapPermanents
                        TapPermanents.MkTapPermanents
                          { TapPermanents.count = 1,
                            TapPermanents.whichPermanents = Filter.HasSubtype Subtype.Wall
                          }
                    ]
                },
          ModifiedRoll.limit = PermissionLimit.Unlimited
        }
      " {\"modifier\":{\"type\":\"Reroll\"},\"cost\":{\"mana\":[],\"components\":[{\"type\":\"TapPermanents\",\"value\":{\"count\":1,\"whichPermanents\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Wall\"}}}}]}} "
  -- CR 706.2b's second step with a life cost and a budget, the wire form
  -- data/cards/night-shift-of-the-living-dead.json writes.
  Spec.it s "MkModifiedRoll, Night Shift of the Living Dead's budgeted adjustment" $
    Common.assertCodec
      s
      ModifiedRoll.codec
      ModifiedRoll.MkModifiedRoll
        { ModifiedRoll.sides = Nothing,
          ModifiedRoll.natural = Nothing,
          ModifiedRoll.modifier = RollModifier.IncreaseOrDecrease 1,
          ModifiedRoll.cost = Just Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost []), Cost.components = [CostComponent.PayLife 1]},
          ModifiedRoll.limit = PermissionLimit.OnceEachTurn
        }
      " {\"modifier\":{\"type\":\"IncreaseOrDecrease\",\"value\":1},\"cost\":{\"mana\":[],\"components\":[{\"type\":\"PayLife\",\"value\":1}]},\"limit\":{\"type\":\"OnceEachTurn\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ModifiedRoll.codec

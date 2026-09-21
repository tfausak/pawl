module Pawl.Codec.ModifiedRollSpec where

import qualified Pawl.Codec.ModifiedRoll as ModifiedRoll
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ModifiedRoll as ModifiedRoll
import qualified Pawl.Types.RollModifier as RollModifier

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
          ModifiedRoll.modifier = RollModifier.Reroll
        }
      " {\"sides\":6,\"natural\":3,\"modifier\":{\"type\":\"Reroll\"}} "
  -- Both narrowings default away, which is Wall of Fortune's bare "a die".
  Spec.it s "MkModifiedRoll narrowing nothing" $
    Common.assertCodec
      s
      ModifiedRoll.codec
      ModifiedRoll.MkModifiedRoll
        { ModifiedRoll.sides = Nothing,
          ModifiedRoll.natural = Nothing,
          ModifiedRoll.modifier = RollModifier.Reroll
        }
      " {\"modifier\":{\"type\":\"Reroll\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ModifiedRoll.codec

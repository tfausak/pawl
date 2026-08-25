module Pawl.Codec.IncreaseActivationCostSpec where

import qualified Pawl.Codec.IncreaseActivationCost as IncreaseActivationCost
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.IncreaseActivationCost as IncreaseActivationCost

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.IncreaseActivationCost" $ do
  -- CR 601.2f through CR 602.2b, as Oppressive Rays taxes the enchanted
  -- creature's activations. CR 303.4b's atom is the whole of that criterion.
  Spec.it s "MkIncreaseActivationCost" $
    Common.assertCodec
      s
      IncreaseActivationCost.codec
      ( IncreaseActivationCost.MkIncreaseActivationCost
          { IncreaseActivationCost.whichAbilities = Filter.IsHostOfSource,
            IncreaseActivationCost.amount = 3
          }
      )
      " {\"whichAbilities\":{\"type\":\"IsHostOfSource\"},\"amount\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s IncreaseActivationCost.codec

module Pawl.Codec.ReinforceSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Reinforce as Reinforce
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Reinforce as Reinforce

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Reinforce.Reinforce Keyword.Keyword)
codec = Reinforce.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Reinforce" $ do
  -- CR 702.77a: "Reinforce 2--{1}". The amount and the cost are independent halves of the printing.
  Spec.it s "MkReinforce" $
    Common.assertCodec
      s
      codec
      ( Reinforce.MkReinforce
          { Reinforce.amount = 2,
            Reinforce.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]), Cost.components = []}
          }
      )
      " {\"amount\":2,\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec

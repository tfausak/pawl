module Pawl.Codec.TapForTotalPowerSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.TapForTotalPower as TapForTotalPower
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (TapForTotalPower.TapForTotalPower Keyword.Keyword)
codec = TapForTotalPower.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TapForTotalPower" $ do
  -- CR 702.122a: crew 6. The 6 is a THRESHOLD on an aggregate, not a count -- the key name is where that differs from Sacrifice.
  Spec.it s "MkTapForTotalPower" $
    Common.assertCodec
      s
      codec
      ( TapForTotalPower.MkTapForTotalPower
          { TapForTotalPower.totalPower = 6,
            TapForTotalPower.whichPermanents = Filter.HasCardType CardType.Creature
          }
      )
      " {\"totalPower\":6,\"whichPermanents\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec

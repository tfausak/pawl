module Pawl.Codec.TapPermanentsSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.TapPermanents as TapPermanents
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TapPermanents as TapPermanents

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (TapPermanents.TapPermanents Keyword.Keyword)
codec = TapPermanents.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TapPermanents" $ do
  -- Springleaf Drum's one creature. The count is HOW MANY, matched exactly --
  -- the key name is where this differs from TapForTotalPower's threshold.
  Spec.it s "MkTapPermanents" $
    Common.assertCodec
      s
      codec
      ( TapPermanents.MkTapPermanents
          { TapPermanents.count = 1,
            TapPermanents.whichPermanents = Filter.HasCardType CardType.Creature,
            TapPermanents.sharingACreatureType = False
          }
      )
      " {\"count\":1,\"whichPermanents\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  -- Weight of Conscience's pair, which must share a creature type (CR 205.3m).
  Spec.it s "sharingACreatureType" $
    Common.assertCodec
      s
      codec
      ( TapPermanents.MkTapPermanents
          { TapPermanents.count = 2,
            TapPermanents.whichPermanents = Filter.HasCardType CardType.Creature,
            TapPermanents.sharingACreatureType = True
          }
      )
      " {\"count\":2,\"whichPermanents\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"sharingACreatureType\":true} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec

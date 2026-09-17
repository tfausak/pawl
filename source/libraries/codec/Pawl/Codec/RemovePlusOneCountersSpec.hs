module Pawl.Codec.RemovePlusOneCountersSpec where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.RemovePlusOneCounters as RemovePlusOneCounters
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.RemovePlusOneCounters as RemovePlusOneCounters

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (RemovePlusOneCounters.RemovePlusOneCounters Keyword.Keyword)
codec = RemovePlusOneCounters.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RemovePlusOneCounters" $ do
  -- Zameck Guildmage's one counter. The count is COUNTERS, and the key name is
  -- SINGULAR -- where TapPermanents' `whichPermanents` narrows a set of objects,
  -- this narrows the one permanent they come off.
  Spec.it s "MkRemovePlusOneCounters" $
    Common.assertCodec
      s
      codec
      ( RemovePlusOneCounters.MkRemovePlusOneCounters
          { RemovePlusOneCounters.count = 1,
            RemovePlusOneCounters.whichPermanent = Filter.HasCardType CardType.Creature
          }
      )
      " {\"count\":1,\"whichPermanent\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec

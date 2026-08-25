module Pawl.Codec.PermanentsGetCountersSpec where

import qualified Pawl.Codec.PermanentsGetCounters as PermanentsGetCounters
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PermanentsGetCounters as PermanentsGetCounters

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermanentsGetCounters" $ do
  -- CR 603.2c's batch placement: a counter kind and the permanents it counts on,
  -- which is what "one or more -1/-1 counters are put on one or more creatures"
  -- names.
  Spec.it s "MkPermanentsGetCounters, both keys" $
    Common.assertCodec
      s
      PermanentsGetCounters.codec
      ( PermanentsGetCounters.MkPermanentsGetCounters
          { PermanentsGetCounters.kind = CounterKind.MinusOneMinusOne,
            PermanentsGetCounters.permanents = Filter.HasCardType CardType.Creature
          }
      )
      " {\"kind\":{\"type\":\"MinusOneMinusOne\"},\"permanents\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentsGetCounters.codec

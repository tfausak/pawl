module Pawl.Codec.CounterPlacementSpec where

import qualified Pawl.Codec.CounterPlacement as CounterPlacement
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPlacement as CounterPlacement
import qualified Pawl.Types.Filter as Filter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounterPlacement" $ do
  -- CR 603.2c's batch placement: a counter kind and the permanents it counts on,
  -- which is what "one or more -1/-1 counters are put on one or more creatures"
  -- names.
  Spec.it s "MkCounterPlacement, both keys" $
    Common.assertCodec
      s
      CounterPlacement.codec
      ( CounterPlacement.MkCounterPlacement
          { CounterPlacement.kind = CounterKind.MinusOneMinusOne,
            CounterPlacement.permanents = Filter.HasCardType CardType.Creature
          }
      )
      " {\"kind\":{\"type\":\"MinusOneMinusOne\"},\"permanents\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CounterPlacement.codec

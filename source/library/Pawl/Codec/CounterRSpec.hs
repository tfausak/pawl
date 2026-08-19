module Pawl.Codec.CounterRSpec where

import qualified Pawl.Codec.CounterR as CounterR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Scaling as Scaling

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounterR" $ do
  -- CR 614.16: Hardened Scales adds one more +1/+1 counter.
  Spec.it s "MkCounterR" $
    Common.assertCodec
      s
      CounterR.codec
      ( CounterR.MkCounterR
          { CounterR.matching =
              CounterPattern.MkCounterPattern
                { CounterPattern.whichKind = Just CounterKind.PlusOnePlusOne,
                  CounterPattern.byWhom = Nothing,
                  CounterPattern.whose = ControllerRelation.Yours,
                  CounterPattern.onWhat = Filter.HasCardType CardType.Creature,
                  CounterPattern.onWho = Nothing
                },
            CounterR.scaling = Scaling.AddMore 1
          }
      )
      " {\"matching\":{\"whichKind\":{\"type\":\"PlusOnePlusOne\"},\"whose\":{\"type\":\"Yours\"},\"onWhat\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"scaling\":{\"type\":\"AddMore\",\"value\":1}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CounterR.codec

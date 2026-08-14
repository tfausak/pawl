{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TurnUpRSpec where

import qualified Pawl.Codec.TurnUpR as TurnUpR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.WithCounters as WithCounters

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TurnUpR" $ do
  -- CR 702.37b, as a megamorph creature turns face up.
  Spec.it s "MkTurnUpR" $
    Common.assertCodec
      s
      TurnUpR.codec
      ( TurnUpR.MkTurnUpR
          { TurnUpR.matching = Filter.IsSource,
            TurnUpR.rewrite =
              TurnUpRewrite.WithCounters
                WithCounters.MkWithCounters
                  { WithCounters.kind = CounterKind.PlusOnePlusOne,
                    WithCounters.amount = 1
                  }
          }
      )
      """ {"matching":{"type":"IsSource"},"rewrite":{"type":"WithCounters","value":{"kind":{"type":"PlusOnePlusOne"},"amount":1}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s TurnUpR.codec

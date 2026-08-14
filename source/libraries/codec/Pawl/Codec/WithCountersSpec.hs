{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.WithCountersSpec where

import qualified Pawl.Codec.WithCounters as WithCounters
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.WithCounters as WithCounters

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.WithCounters" $ do
  -- CR 614.1c, as a modular or graft permanent enters.
  Spec.it s "MkWithCounters, both keys" $
    Common.assertCodec
      s
      WithCounters.codec
      (WithCounters.MkWithCounters {WithCounters.kind = CounterKind.PlusOnePlusOne, WithCounters.amount = 2})
      """ {"kind":{"type":"PlusOnePlusOne"},"amount":2} """
  Spec.it s "has a schema" $ Common.assertHasSchema s WithCounters.codec

{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TurnUpRewriteSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.TurnUpRewrite" . Spec.it s "WithCounters (megamorph, CR 702.37b)" $
    Common.assertJsonCodec
      s
      TurnUpRewrite.toJson
      TurnUpRewrite.fromJson
      (TurnUpRewrite.WithCounters CounterKind.PlusOnePlusOne 1)
      """ {"type":"WithCounters","value":[{"type":"PlusOnePlusOne"},1]} """

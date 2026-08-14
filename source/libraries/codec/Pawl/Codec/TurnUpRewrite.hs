module Pawl.Codec.TurnUpRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.WithCounters as WithCounters
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite

-- | The kind-and-count pair, named once so both directions read the same order
-- out of the two-element array.
counters :: Codec.Codec (CounterKind.CounterKind Keyword.Keyword, Natural.Natural)
counters = Common.tuple (CounterKind.codec Keyword.codec) Common.natural

codec :: Codec.Codec TurnUpRewrite.TurnUpRewrite
codec =
  Arm.tagged
    encode
    [ Arm.payload "WithCounters" WithCounters.codec TurnUpRewrite.WithCounters,
      Arm.payload "MayAttachTo" filterCodec TurnUpRewrite.MayAttachTo
    ]
  where
    filterCodec = Filter.codec Keyword.codec
    encode r = case r of
      TurnUpRewrite.WithCounters x -> Common.tagged "WithCounters" . Just $ Codec.encode WithCounters.codec x
      TurnUpRewrite.MayAttachTo f -> Common.tagged "MayAttachTo" . Just $ Codec.encode filterCodec f

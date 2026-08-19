module Pawl.Codec.TurnUpRewrite where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.WithCounters as WithCounters
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite

codec :: Codec.Codec TurnUpRewrite.TurnUpRewrite
codec =
  Arm.tagged
    [ Arm.payload "WithCounters" WithCounters.codec TurnUpRewrite.WithCounters (\x -> case x of TurnUpRewrite.WithCounters y -> Just y; _ -> Nothing),
      Arm.payload "MayAttachTo" filterCodec TurnUpRewrite.MayAttachTo (\x -> case x of TurnUpRewrite.MayAttachTo y -> Just y; _ -> Nothing)
    ]
  where
    filterCodec = Filter.codec Keyword.codec

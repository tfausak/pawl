module Pawl.Codec.UntapRewrite where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.UntapRewrite as UntapRewrite

codec :: Codec.Codec UntapRewrite.UntapRewrite
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "RemoveStunCounter" UntapRewrite.RemoveStunCounter,
      Arm.payload "RemoveCounterToUntap" (CounterKind.codec Keyword.codec) UntapRewrite.RemoveCounterToUntap (\x -> case x of UntapRewrite.RemoveCounterToUntap y -> Just y; _ -> Nothing)
    ]

tagOf :: UntapRewrite.UntapRewrite -> String
tagOf x = case x of
  UntapRewrite.RemoveStunCounter -> "RemoveStunCounter"
  UntapRewrite.RemoveCounterToUntap {} -> "RemoveCounterToUntap"

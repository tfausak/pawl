module Pawl.Codec.SpecialAction where

import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SpecialAction as SpecialAction

codec :: Codec.Codec SpecialAction.SpecialAction
codec =
  Arm.tagged
    encode
    [ Arm.nullary "DiscardThisAnyTime" SpecialAction.DiscardThisAnyTime,
      Arm.payload "IgnoreThisUntilEndOfTurn" costCodec SpecialAction.IgnoreThisUntilEndOfTurn
    ]
  where
    costCodec = Cost.codec Keyword.codec
    encode a = case a of
      SpecialAction.DiscardThisAnyTime -> Common.nullary "DiscardThisAnyTime"
      SpecialAction.IgnoreThisUntilEndOfTurn c -> Common.tagged "IgnoreThisUntilEndOfTurn" . Just $ Codec.encode costCodec c
